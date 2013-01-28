{-# LANGUAGE OverloadedStrings #-}
module WebsocketBackend
    ( websocketBackend
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception (fromException)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Crypto.PasswordStore
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import System.Random

import qualified Data.Attoparsec as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.BitcoinRPC as RPC
import qualified Network.WebSockets as WS

import ClientHub
import CommonTypes
import Config
import DbUtils
import LoggingUtils
import PendingActionsTracker

bridgewalkerServerVersion :: T.Text
bridgewalkerServerVersion = "0.1"

base58 :: String
base58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

guestNameLength :: Int
guestNameLength = 8

guestPwLength :: Int
guestPwLength = 25

passwordStoreStrength :: Int
passwordStoreStrength = 15

data WebsocketCommand = WSSendBTC { wcSessionID :: T.Text
                                  , wcBitcoinAddress :: T.Text
                                  , wcAmount :: Integer
                                  }
                      | WSRequestVersion { wcClientVersion :: T.Text }
                      | WSCreateGuestAccount
                      | WSLogin { wcAccountName :: T.Text
                                , wcAccountPassword :: T.Text
                                }
                      | WSRequestStatus
                      | WSRequestQuote { wcRequestID :: Integer
                                       , wcQuoteType :: QuoteType
                                       }
                      | WSPing
                      deriving (Show)

data WebsocketReply = WSStatus { wrStatus :: ClientStatus }
                    | WSCommandNotUnderstood { wrInfo :: T.Text }
                    | WSCommandNotAvailable { wrInfo :: T.Text }
                    | WSNeedToBeAuthenticated
                    | WSServerVersion { wrServerVersion :: T.Text }
                    | WSGuestAccountCreated { wrAccountName :: T.Text
                                            , wrAccountPassword :: T.Text
                                            }
                    | WSLoginSuccessful
                    | WSLoginFailed { wrReason :: T.Text }
                    | WSQuoteUnavailable { wrRequestID :: Integer }
                    | WSQuote { wrRequestID :: Integer
                              , wrQuote :: QuoteData
                              }
                    | WSPong
                    deriving (Show)

data AuthenticatedEvent = MessageFromClient WebsocketCommand
                        | MessageFromClientHub ClientHubAnswer

instance FromJSON WebsocketCommand
  where
    parseJSON (Object o) = case H.lookup "op" o of
        Just "request_status" -> return WSRequestStatus
        Just "request_quote" -> do
            t <- o .: "type" :: Parser T.Text
            i <- o .: "request_id"
            a <- o .: "amount"
            case t of
                "quote_based_on_btc" ->
                    return $ WSRequestQuote i (QuoteBasedOnBTC a)
                "quote_based_on_usd_before_fees" ->
                    return $ WSRequestQuote i (QuoteBasedOnUSDBeforeFees a)
                "quote_based_on_usd_after_fees" ->
                    return $ WSRequestQuote i (QuoteBasedOnUSDAfterFees a)
                _ -> mzero
        Just "send_btc" -> WSSendBTC <$> o .: "session_id"
                                     <*> o .: "bitcoin_address"
                                     <*> o .: "amount"
        Just "request_version" -> WSRequestVersion <$> o .: "client_version"
        Just "create_guest_account" -> return WSCreateGuestAccount
        Just "login" -> WSLogin <$> o .: "account_name"
                                <*> o .: "account_password"
        Just "ping" -> return WSPing
        Just _ -> mzero
        Nothing -> mzero
    parseJSON _ = mzero

instance ToJSON WebsocketReply
  where
    toJSON (WSStatus status) =
        object [ "reply" .= ("status" :: T.Text)
               , "status" .= status
               ]
    toJSON (WSCommandNotUnderstood info) =
        object [ "reply" .= ("not_understood" :: T.Text)
               , "info" .= info
               ]
    toJSON (WSCommandNotAvailable info) =
        object [ "reply" .= ("not_available" :: T.Text)
               , "info" .= info
               ]
    toJSON WSNeedToBeAuthenticated =
        object [ "reply" .= ("need_to_be_authenticated" :: T.Text) ]
    toJSON (WSServerVersion serverVersion) =
        object [ "reply" .= ("server_version" :: T.Text)
               , "server_version" .= serverVersion
               ]
    toJSON (WSGuestAccountCreated accountName accountPw) =
        object [ "reply" .= ("guest_account_created" :: T.Text)
               , "account_name" .= accountName
               , "account_password" .= accountPw
               ]
    toJSON WSLoginSuccessful =
        object [ "reply" .= ("login_successful" :: T.Text) ]
    toJSON (WSLoginFailed reason) =
        object [ "reply" .= ("login_failed" :: T.Text)
               , "reason" .= reason
               ]
    toJSON (WSQuoteUnavailable requestID) =
        object [ "reply" .= ("quote_unavailable" :: T.Text)
               , "request_id" .= requestID
               ]
    toJSON (WSQuote requestID quoteData) =
        object [ "reply" .= ("quote" :: T.Text)
               , "request_id" .= requestID
               , "btc" .= qdBTC quoteData
               , "usd_recipient" .= qdUSDRecipient quoteData
               , "usd_account" .= qdUSDAccount quoteData
               , "sufficient_balance" .= qdSufficientBalance quoteData
               ]
    toJSON WSPong =
        object [ "reply" .= ("pong" :: T.Text) ]

--broadcast :: Text -> ServerState -> IO ()
--broadcast message clients = do
--    T.putStrLn message
--    forM_ clients $ \(_, sink) -> WS.sendSink sink $ WS.textData message
--
--main :: IO ()
--main = do
--    state <- newMVar newServerState
--    WS.runServer "0.0.0.0" 9160 $ application state

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

prepareWSReply :: ToJSON a => a -> T.Text
prepareWSReply = T.decodeUtf8 . toStrict . encode

parseWSCommand :: FromJSON b => T.Text -> Either String b
parseWSCommand cmd =
    let parse = AP.parseOnly (fromJSON <$> json) $ T.encodeUtf8 cmd
    in case parse of
        Left _ -> Left "Malformed JSON"
        Right (Error _) -> Left "Malformed command"
        Right (Success d) -> Right d

websocketBackend :: BridgewalkerHandles -> WS.Request -> WS.WebSockets WS.Hybi00 ()
websocketBackend bwHandles rq = do
    WS.acceptRequest rq
    forever $ processMessages bwHandles

processMessages :: WS.TextProtocol p => BridgewalkerHandles -> WS.WebSockets p ()
processMessages bwHandles = do
    let dbConn = bhDBConnCH bwHandles
        patHandle = bhPendingActionsTrackerHandle bwHandles
        chHandle = bhClientHubHandle bwHandles
    msg <- WS.receiveData
    let cmd = parseWSCommand msg :: Either String WebsocketCommand
    case parseWSCommand msg of
        Left errMsg ->
            WS.sendTextData . prepareWSReply $
                                    WSCommandNotUnderstood (T.pack errMsg)
        Right cmd -> case cmd of
            WSRequestVersion _ -> do    -- ignore client version for now,
                                        -- but might be needed in the future
                let reply = WSServerVersion bridgewalkerServerVersion
                WS.sendTextData . prepareWSReply $ reply
            WSCreateGuestAccount -> do
                (guestName, guestPw) <- liftIO $ createGuestAccount bwHandles
                let reply = WSGuestAccountCreated guestName guestPw
                WS.sendTextData . prepareWSReply $ reply
            WSLogin accountName accountPassword -> do
                accountM <- liftIO $
                    checkLogin dbConn accountName accountPassword
                case accountM of
                    Nothing ->
                        WS.sendTextData . prepareWSReply $
                            WSLoginFailed "Unkown account or wrong password"
                    Just account -> do
                        WS.sendTextData . prepareWSReply $ WSLoginSuccessful
                        answerChan <- liftIO $
                                        registerClientWithHub chHandle account
                        combinationChan <- liftIO newChan
                        sink <- WS.getSink
                        _ <- liftIO . forkIO $ continueAuthenticated
                                                combinationChan sink
                                                chHandle account
                        _ <- liftIO . forkIO $ forwardClientHubAnswers
                                                    answerChan combinationChan
                        processMessagesAuthenticated combinationChan
            _ -> do
                WS.sendTextData . prepareWSReply $ WSNeedToBeAuthenticated

processMessagesAuthenticated :: WS.TextProtocol p => Chan AuthenticatedEvent -> WS.WebSockets p b
processMessagesAuthenticated combinationChan = forever $ do
    msg <- WS.receiveData
    let cmd = parseWSCommand msg :: Either String WebsocketCommand
    case parseWSCommand msg of
        Left errMsg ->
            WS.sendTextData . prepareWSReply $
                                    WSCommandNotUnderstood (T.pack errMsg)
        Right cmd -> liftIO $ writeChan combinationChan (MessageFromClient cmd)

forwardClientHubAnswers :: Chan ClientHubAnswer -> Chan AuthenticatedEvent -> IO ()
forwardClientHubAnswers answerChan combinationChan = forever $ do
    answer <- readChan answerChan
    writeChan combinationChan $ MessageFromClientHub answer

continueAuthenticated :: WS.TextProtocol p =>Chan AuthenticatedEvent-> WS.Sink p -> ClientHubHandle -> BridgewalkerAccount -> IO ()
continueAuthenticated combinationChan sink chHandle account = forever $ do
    combiMsg <- readChan combinationChan
    case combiMsg of
        MessageFromClient msg -> case msg of
            WSRequestStatus -> requestClientStatus chHandle account
            WSPing -> receivedPing chHandle account
            WSRequestQuote reqID quoteType ->
                requestQuote chHandle account reqID quoteType
            _ -> let wsData = WS.textData . prepareWSReply $
                                WSCommandNotAvailable "Command not available\
                                                      \ after login."
                 in WS.sendSink sink wsData
        MessageFromClientHub msg -> case msg of
            ForwardStatusToClient status ->
                let wsData = WS.textData . prepareWSReply $ WSStatus status
                in WS.sendSink sink wsData  -- TODO: check for exceptions
                                            -- (or maybe already handled by Snap?)
            ForwardQuoteToClient reqID Nothing ->
                let wsData = WS.textData . prepareWSReply $
                                                WSQuoteUnavailable reqID
                in WS.sendSink sink wsData
            ForwardQuoteToClient reqID (Just replyData) ->
                let wsData = WS.textData . prepareWSReply $
                                                WSQuote reqID replyData
                in WS.sendSink sink wsData
            SendPongToClient ->
                let wsData = WS.textData . prepareWSReply $ WSPong
                in WS.sendSink sink wsData
            CloseConnectionWithClient ->
                WS.sendSink sink $ WS.close ("Timeout" :: T.Text)


--            WSRequestStatus _ _ -> do   -- TODO: check hash
--                status <- liftIO $ compileClientStatus bwHandles magicAccount
--                let reply = WSStatusReply { wrStatus = status
--                                          , wrStatusHash = "TODO"
--                                          }
--                WS.sendTextData . prepareWSReply $ reply
--            WSSendBTC _ address amount -> do
--                -- TODO: move functionality into ClientHub module
--                -- TODO: check validity of Bitcoin address
--                -- TODO: protect against negative or too large amounts
--                --       (probably need to modify DepthStore to signal
--                --        when amount can not be fulfilled)
--                let action = BuyBTCAction
--                                { baAmount = amount
--                                , baAddress =
--                                    RPC.BitcoinAddress address
--                                , baAccount = magicAccount
--                                }
--                liftIO $ addBuyAction dbConn action
--                liftIO $ nudgePendingActionsTracker patHandle

--createGuestAccount :: Connection -> IO (T.Text, T.Text)
--createGuestAccount dbConn = do

createGuestAccount bwHandles = do
    let dbConn = bhDBConnCH bwHandles
        watchdogLogger = bhWatchdogLogger bwHandles
        rpcAuth = bcRPCAuth . bhConfig $ bwHandles
        logger = bhAppLogger bwHandles
    guestName <- createUniqueGuestName dbConn
    guestPw <- randomText guestPwLength
    pwHash <- makePassword (T.encodeUtf8 guestPw) passwordStoreStrength
    btcAddress <- RPC.getNewAddressR (Just watchdogLogger) rpcAuth
    execute dbConn
        "insert into accounts (btc_in, usd_balance, account_name, account_pw\
            \, is_full_account, primary_btc_address)\
            \ values (0, 0, ?, ?, false, ?)"
            (guestName, pwHash, RPC.btcAddress btcAddress)
    account <- getAccountNumber dbConn guestName
    execute dbConn
        "insert into addresses (account, btc_address) values (?, ?)"
            (account, RPC.btcAddress btcAddress)
    let logMsg = GuestAccountCreated (T.unpack guestName)
    logger logMsg
    return (guestName, guestPw)

createUniqueGuestName :: Connection -> IO T.Text
createUniqueGuestName dbConn = do
    guestName <- ("guest_" `T.append`) <$> randomText guestNameLength
    exists <- checkGuestNameExists dbConn guestName
    if exists
        then createUniqueGuestName dbConn
        else return guestName

randomText :: Int -> IO T.Text
randomText len = do
    let size = length base58
    g <- newStdGen
    let ints = take len $ randomRs (0, size - 1) g
        chars = map (\pos -> base58 !! pos) ints
    return $ T.pack chars

addBuyAction dbConn action = withTransaction dbConn $ do
    paState <- readPendingActionsStateFromDB dbConn
    let paState' = addPendingActions paState [action]
    writePendingActionsStateToDB dbConn paState'

--runWebsocketServer :: BridgewalkerHandles -> IO ()
--runWebsocketServer bwHandles =
--    WS.runServer "0.0.0.0" 9160 $ webSocketApp bwHandles


--application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
--application state rq = do
--    WS.acceptRequest rq
--    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
--    sink <- WS.getSink
--    msg <- WS.receiveData
--    clients <- liftIO $ readMVar state
--    case msg of
--        _   | not (prefix `T.isPrefixOf` msg) ->
--                WS.sendTextData ("Wrong announcement" :: Text)
--            | any ($ fst client)
--                [T.null, T.any isPunctuation, T.any isSpace] ->
--                    WS.sendTextData ("Name cannot " `mappend`
--                        "contain punctuation or whitespace, and " `mappend`
--                        "cannot be empty" :: Text)
--            | clientExists client clients ->
--                WS.sendTextData ("User already exists" :: Text)
--            | otherwise -> do
--               liftIO $ modifyMVar_ state $ \s -> do
--                   let s' = addClient client s
--                   WS.sendSink sink $ WS.textData $
--                       "Welcome! Users: " `mappend`
--                       T.intercalate ", " (map fst s)
--                   broadcast (fst client `mappend` " joined") s'
--                   return s'
--               talk state client
--          where
--            prefix = "Hi! I am "
--            client = (T.drop (T.length prefix) msg, sink)
--
--talk :: WS.Protocol p => MVar ServerState -> Client -> WS.WebSockets p ()
--talk state client@(user, _) = flip WS.catchWsError catchDisconnect $ do
--    msg <- WS.receiveData
--    liftIO $ readMVar state >>= broadcast
--        (user `mappend` ": " `mappend` msg)
--    talk state client
--  where
--    catchDisconnect e = case fromException e of
--        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
--            let s' = removeClient client s
--            broadcast (user `mappend` " disconnected") s'
--            return s'
--        _ -> return ()
