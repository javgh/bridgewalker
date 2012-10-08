{-# LANGUAGE OverloadedStrings #-}
module WebsocketPrototype where

import Control.Applicative
import Control.Concurrent
import Control.Exception (fromException)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Database.PostgreSQL.Simple

import qualified Data.Attoparsec as AP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.BitcoinRPC as RPC
import qualified Network.WebSockets as WS

import PendingActionsTracker
import DbUtils

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

data ClientStatus = ClientStatus { csUSDBalance :: Integer }
                    deriving (Show)

data WebsocketCommand = WSRequestStatus { wcSessionID :: T.Text
                                        , wcStatusHash :: Maybe T.Text
                                        }
                      | WSSendBTC { wcSessionID :: T.Text
                                  , wcBitcoinAddress :: T.Text
                                  , wcAmount :: Integer
                                  }
                      deriving (Show)

data WebsocketReply = WSStatusReply { wrStatus :: ClientStatus
                                    , wrStatusHash :: T.Text
                                    }
                    | WSStatusUnchangedReply
                    | WSCommandNotUnderstood { wrInfo :: T.Text }
                    deriving (Show)

instance FromJSON WebsocketCommand
  where
    parseJSON (Object o) = case H.lookup "op" o of
        Just "status" -> WSRequestStatus <$> o .: "session_id"
                                         <*> o .:? "status_hash"
        Just "send_btc" -> WSSendBTC <$> o .: "session_id"
                                     <*> o .: "bitcoin_address"
                                     <*> o .: "amount"
        Just _ -> mzero
        Nothing -> mzero
    parseJSON _ = mzero

instance ToJSON ClientStatus
  where
    toJSON cs@ClientStatus{} =
        let usdBalance = csUSDBalance cs
        in object [ "usd_balance" .= usdBalance ]

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

webSocketApp :: Connection -> WS.Request -> WS.WebSockets WS.Hybi00 ()
webSocketApp dbConn rq = do
    WS.acceptRequest rq
    forever $ processMessages dbConn

processMessages dbConn = do
    msg <- WS.receiveData
    let cmd = parseWSCommand msg :: Either String WebsocketCommand
    case parseWSCommand msg of
        Left errMsg ->
            WS.sendTextData . prepareWSReply $
                                    WSCommandNotUnderstood (T.pack errMsg)
        Right cmd -> case cmd of
            WSRequestStatus _ _ -> do   -- TODO: check hash
                usdBalance <- liftIO $ getUSDBalance dbConn 1
                let status = ClientStatus usdBalance
                    reply = WSStatusReply { wrStatus = status
                                          , wrStatusHash = "TODO"
                                          }
                WS.sendTextData . prepareWSReply $ reply
            WSSendBTC _ address amount -> do
                -- TODO: check validity of Bitcoin address
                -- TODO: protect against negative or too large amounts
                --       (probably need to modify DepthStore to signal
                --        when amount can not be fulfilled)
                let action = BuyBTCAction
                                { baAmount = amount
                                , baAddress =
                                    RPC.BitcoinAddress address
                                , baAccount = BridgewalkerAccount 1
                                }
                liftIO $ addBuyAction dbConn action

addBuyAction dbConn action = withTransaction dbConn $ do
    paState <- readPendingActionsStateFromDB dbConn
    let paState' = addPendingActions paState [action]
    writePendingActionsStateToDB dbConn paState'

main = do
    dbConn <- connectPostgreSQL myConnectInfo
    WS.runServer "0.0.0.0" 9160 $ webSocketApp dbConn


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
