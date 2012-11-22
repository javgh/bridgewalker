{-# LANGUAGE OverloadedStrings #-}
module WebsocketPrototype
    ( runWebsocketServer
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception (fromException)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
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
import Config
import DbUtils
import PendingActionsTracker

magicAccount = BridgewalkerAccount 1

bridgewalkerServerVersion :: T.Text
bridgewalkerServerVersion = "0.1"

base58 :: String
base58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

guestNameLength :: Int
guestNameLength = 8

guestPwLength :: Int
guestPwLength = 25

data WebsocketCommand = WSRequestStatus { wcSessionID :: T.Text
                                        , wcStatusHash :: Maybe T.Text
                                        }
                      | WSSendBTC { wcSessionID :: T.Text
                                  , wcBitcoinAddress :: T.Text
                                  , wcAmount :: Integer
                                  }
                      | WSRequestVersion { wcClientVersion :: T.Text }
                      | WSRegisterGuestAccount
                      deriving (Show)

data WebsocketReply = WSStatusReply { wrStatus :: ClientStatus
                                    , wrStatusHash :: T.Text
                                    }
                    | WSStatusUnchangedReply
                    | WSCommandNotUnderstood { wrInfo :: T.Text }
                    | WSServerVersion { wrServerVersion :: T.Text }
                    deriving (Show)

instance FromJSON WebsocketCommand
  where
    parseJSON (Object o) = case H.lookup "op" o of
        Just "status" -> WSRequestStatus <$> o .: "session_id"
                                         <*> o .:? "status_hash"
        Just "send_btc" -> WSSendBTC <$> o .: "session_id"
                                     <*> o .: "bitcoin_address"
                                     <*> o .: "amount"
        Just "version" -> WSRequestVersion <$> o .: "client_version"
        Just "register_guest_account" -> return WSRegisterGuestAccount
        Just _ -> mzero
        Nothing -> mzero
    parseJSON _ = mzero

instance ToJSON WebsocketReply
  where
    toJSON (WSStatusReply status statusHash) =
        object [ "reply" .= ("status_reply" :: T.Text)
               , "status" .= status
               , "status_hash" .= statusHash
               ]
    toJSON WSStatusUnchangedReply =
        object [ "reply" .= ("status_unchanged_reply" :: T.Text) ]
    toJSON (WSCommandNotUnderstood info) =
        object [ "reply" .= ("not_understood" :: T.Text)
               , "info" .= info
               ]
    toJSON (WSServerVersion serverVersion) =
        object [ "reply" .= ("server_version" :: T.Text)
               , "server_version" .= serverVersion
               ]

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

webSocketApp :: BridgewalkerHandles -> WS.Request -> WS.WebSockets WS.Hybi00 ()
webSocketApp bwHandles rq = do
    WS.acceptRequest rq
    forever $ processMessages bwHandles

processMessages bwHandles = do
    let dbConn = bhDBConnCH bwHandles
        patHandle = bhPendingActionsTrackerHandle bwHandles
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
            WSRegisterGuestAccount -> do
                undefined
            WSRequestStatus _ _ -> do   -- TODO: check hash
                status <- liftIO $ compileClientStatus bwHandles magicAccount
                let reply = WSStatusReply { wrStatus = status
                                          , wrStatusHash = "TODO"
                                          }
                WS.sendTextData . prepareWSReply $ reply
            WSSendBTC _ address amount -> do
                -- TODO: move functionality into ClientHub module
                -- TODO: check validity of Bitcoin address
                -- TODO: protect against negative or too large amounts
                --       (probably need to modify DepthStore to signal
                --        when amount can not be fulfilled)
                let action = BuyBTCAction
                                { baAmount = amount
                                , baAddress =
                                    RPC.BitcoinAddress address
                                , baAccount = magicAccount
                                }
                liftIO $ addBuyAction dbConn action
                liftIO $ nudgePendingActionsTracker patHandle

createGuestAccount :: Connection -> IO (T.Text, T.Text)
createGuestAccount dbConn = do
    guestName <- createUniqueGuestName dbConn
    guestPw <- randomText guestPwLength
    -- TODO: scramble password - use pwstore-fast
    execute dbConn
        "insert into accounts (btc_in, usd_balance, account_name, account_pw\
            \, is_full_account, primary_btc_address)\
            \ values (0, 0, ?, ?, false, null)" (guestName, guestPw)
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

runWebsocketServer :: BridgewalkerHandles -> IO ()
runWebsocketServer bwHandles =
    WS.runServer "0.0.0.0" 9160 $ webSocketApp bwHandles


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
