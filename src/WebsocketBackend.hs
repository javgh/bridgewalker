{-# LANGUAGE OverloadedStrings #-}
module WebsocketBackend
    ( websocketBackend
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Crypto.PasswordStore
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import System.FilePath
import System.Random

import qualified Control.Exception as E
import qualified Data.Attoparsec as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified GHC.IO.Exception as E
import qualified Network.BitcoinRPC as RPC
import qualified Network.WebSockets as WS
import qualified System.IO.Error as E

import ClientHub
import CommonTypes
import Config
import DbUtils
import Utils

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

data WebsocketCommand = WSRequestVersion { _wcClientVersion :: T.Text }
                      | WSCreateGuestAccount
                      | WSLogin { _wcAccountName :: T.Text
                                , _wcAccountPassword :: T.Text
                                }
                      | WSRequestStatus
                      | WSRequestQuote { _wcRequestID :: Integer
                                       , _wcAddressM :: Maybe T.Text
                                       , _wcAmountType :: AmountType
                                       }
                      | WSSendPayment { _wcRequestID :: Integer
                                      , _wcAddress :: T.Text
                                      , _wcAmountType :: AmountType
                                      }
                      | WSSubmitClaim { _wcAddress :: T.Text }
                      | WSPing
                      deriving (Show)

data WebsocketReply = WSStatus { _wrStatus :: ClientStatus }
                    | WSCommandNotUnderstood { _wrInfo :: T.Text }
                    | WSCommandNotAvailable { _wrInfo :: T.Text }
                    | WSNeedToBeAuthenticated
                    | WSServerVersion { _wrServerVersion :: T.Text }
                    | WSGuestAccountCreated { _wrAccountName :: T.Text
                                            , _wrAccountPassword :: T.Text
                                            }
                    | WSLoginSuccessful
                    | WSLoginFailed { _wrReason :: T.Text }
                    | WSQuoteUnavailable { _wrRequestID :: Integer }
                    | WSQuote { _wrRequestID :: Integer
                              , _wrQuote :: QuoteData
                              }
                    | WSSendFailed { _wrRequestID :: Integer
                                   , _wrReason :: T.Text
                                   }
                    | WSSendSuccessful { _wrRequestID :: Integer
                                       , _wrSerializedTransactionM
                                            :: Maybe RPC.SerializedTransaction
                                       }
                    | WSPong { _wrExchangeRate :: Integer }
                    deriving (Show)

data AuthenticatedEvent = MessageFromClient WebsocketCommand
                        | MessageFromClientHub ClientHubAnswer

instance FromJSON WebsocketCommand
  where
    parseJSON (Object o) = case H.lookup "op" o of
        Just "request_status" -> return WSRequestStatus
        Just "request_quote" -> do
            i <- o .: "request_id"
            mARaw <- o .:? "address"
            let mA = fmap T.strip mARaw
            t <- o .: "type" :: Parser T.Text
            a <- o .: "amount"
            when (a < 0) mzero  -- only accept positive values
            case t of
                "amount_based_on_btc" ->
                    return $ WSRequestQuote i mA (AmountBasedOnBTC a)
                "amount_based_on_usd_before_fees" ->
                    return $ WSRequestQuote i mA (AmountBasedOnUSDBeforeFees a)
                "amount_based_on_usd_after_fees" ->
                    return $ WSRequestQuote i mA (AmountBasedOnUSDAfterFees a)
                _ -> mzero
        Just "send_payment" -> do
            i <- o .: "request_id"
            addrRaw <- o .: "address" :: Parser T.Text
            let addr = T.strip addrRaw
            t <- o .: "type" :: Parser T.Text
            amount <- o .: "amount"
            when (amount < 0) mzero  -- only accept positive values
            case t of
                "amount_based_on_btc" ->
                    return $ WSSendPayment i addr (AmountBasedOnBTC amount)
                "amount_based_on_usd_before_fees" ->
                    return $ WSSendPayment i addr (AmountBasedOnUSDBeforeFees amount)
                "amount_based_on_usd_after_fees" ->
                    return $ WSSendPayment i addr (AmountBasedOnUSDAfterFees amount)
                _ -> mzero
        Just "submit_claim" -> do
            addrRaw <- o .: "address" :: Parser T.Text
            let addr = T.strip addrRaw
            return $ WSSubmitClaim addr
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
    toJSON (WSSendFailed requestID reason) =
        object [ "reply" .= ("send_failed" :: T.Text)
               , "request_id" .= requestID
               , "reason" .= reason
               ]
    toJSON (WSSendSuccessful requestID Nothing) =
        object [ "reply" .= ("send_successful" :: T.Text)
               , "request_id" .= requestID
               ]
    toJSON (WSSendSuccessful requestID (Just (RPC.SerializedTransaction tx))) =
        object [ "reply" .= ("send_successful" :: T.Text)
               , "request_id" .= requestID
               , "tx" .= tx
               ]
    toJSON (WSPong rate) =
        object [ "reply" .= ("pong" :: T.Text)
               , "exchange_rate" .= rate
               ]

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

prepareWSReply :: ToJSON a => a -> T.Text
prepareWSReply = T.decodeUtf8 . toStrict . encode

parseWSCommand :: FromJSON b => T.Text -> Either String b
parseWSCommand cmd =
    let cmdParse = AP.parseOnly (fromJSON <$> json) $ T.encodeUtf8 cmd
    in case cmdParse of
        Left _ -> Left "Malformed JSON"
        Right (Error _) -> Left "Malformed command"
        Right (Success d) -> Right d

websocketBackend :: BridgewalkerHandles -> WS.Request -> WS.WebSockets WS.Hybi00 ()
websocketBackend bwHandles rq = do
    WS.acceptRequest rq
    flip WS.catchWsError catchDisconnect $ forever (processMessages bwHandles)
  where
    catchDisconnect e = case E.fromException e of
        Just WS.ConnectionClosed -> return ()   -- clients might disappear on us,
                                                -- so ignore these errors;
        _ -> WS.throwWsError e                  -- everything else is re-thrown
                                                -- to be reported by Snap

processMessages :: WS.TextProtocol p => BridgewalkerHandles -> WS.WebSockets p ()
processMessages bwHandles = do
    let dbConn = bhDBConnCH bwHandles
        chHandle = bhClientHubHandle bwHandles
    msg <- WS.receiveData
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
                        _ <- liftIO . linkedForkIO $ continueAuthenticated
                                                        combinationChan sink
                                                        chHandle account
                        _ <- liftIO . linkedForkIO $
                                        forwardClientHubAnswers
                                            answerChan combinationChan
                        processMessagesAuthenticated combinationChan
            _ -> WS.sendTextData . prepareWSReply $ WSNeedToBeAuthenticated

processMessagesAuthenticated :: WS.TextProtocol p => Chan AuthenticatedEvent -> WS.WebSockets p b
processMessagesAuthenticated combinationChan = forever $ do
    msg <- WS.receiveData
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
continueAuthenticated combinationChan sink chHandle account =
    flip E.catchIOError badFileDescriptor $ forever $ do
        combiMsg <- readChan combinationChan
        case combiMsg of
            MessageFromClient msg -> case msg of
                WSRequestStatus -> requestClientStatus chHandle account
                WSPing -> receivedPing chHandle account
                WSRequestQuote reqID mAddress amountType ->
                    requestQuote chHandle account reqID mAddress amountType
                WSSendPayment reqID address amountType ->
                    sendPayment chHandle account reqID address amountType
                WSSubmitClaim address -> do
                    let claimFile = "./claims" </> "account_"
                            ++ (show . bAccount) account
                            ++ "_" ++ T.unpack address
                    appendFile claimFile $ (show . bAccount) account
                                                ++ "\t" ++ T.unpack address
                                                ++ "\n"
                _ -> let wsData = WS.textData . prepareWSReply $
                            WSCommandNotAvailable "Command not available\
                                                   \ after login."
                     in WS.sendSink sink wsData
            MessageFromClientHub msg ->
                let wsData =
                        case msg of
                            ForwardStatusToClient status ->
                                WS.textData . prepareWSReply $ WSStatus status
                            ForwardQuoteToClient reqID Nothing ->
                                WS.textData . prepareWSReply $
                                                    WSQuoteUnavailable reqID
                            ForwardQuoteToClient reqID (Just replyData) ->
                                WS.textData . prepareWSReply $
                                                   WSQuote reqID replyData
                            ForwardSuccessfulSend reqID mTx ->
                                WS.textData . prepareWSReply $
                                   WSSendSuccessful reqID mTx
                            ForwardFailedSend reqID reason ->
                                WS.textData . prepareWSReply $
                                   WSSendFailed reqID reason
                            SendPongToClient rate ->
                                WS.textData . prepareWSReply $ WSPong rate
                            CloseConnectionWithClient ->
                                WS.close ("Timeout" :: T.Text)
                in WS.sendSink sink wsData

-- Clients might disappear on us, resulting in "invalid argument (Bad file
-- descriptor). We can ignore those errors - everything else is re-thrown and
-- appears on stderr.
badFileDescriptor :: IOError -> IO ()
badFileDescriptor e =
    unless (E.ioeGetErrorType e == E.InvalidArgument) $ ioError e

createGuestAccount :: BridgewalkerHandles -> IO (T.Text, T.Text)
createGuestAccount bwHandles = do
    let dbConn = bhDBConnCH bwHandles
        dbLock = bhDBWriteLock bwHandles
        watchdogLogger = bhWatchdogLogger bwHandles
        rpcAuth = bcRPCAuth . bhConfig $ bwHandles
        logger = bhAppLogger bwHandles
    guestName <- createUniqueGuestName dbConn
    guestPw <- randomText guestPwLength
    pwHash <- makePassword (T.encodeUtf8 guestPw) passwordStoreStrength
    btcAddress <- RPC.getNewAddressR (Just watchdogLogger) rpcAuth
    _ <- withSerialTransaction dbLock dbConn $ do
            _ <- execute dbConn
                    "insert into accounts (btc_in, usd_balance\
                        \, account_name, account_pw\
                        \, is_full_account, primary_btc_address)\
                        \ values (0, 0, ?, ?, false, ?)"
                        (guestName, pwHash, RPC.btcAddress btcAddress)
            account <- getAccountNumber dbConn guestName
            execute dbConn
                "insert into addresses (account, btc_address)\
                    \ values (?, ?)"
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
