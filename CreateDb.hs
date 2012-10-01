{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Network.BitcoinRPC.Events.MarkerAddresses
import Data.Serialize

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import PendingActionsTracker

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

main :: IO ()
main = connectPostgreSQL myConnectInfo >>= \conn -> do
    execute_ conn
        "create table states (key text primary key, state text)"
    let fetStateStr = B64.encode . encode $ initialFilteredEventTaskState
    execute conn
        "insert into states values (?, ?)"
        ("filteredeventtaskstate" :: B.ByteString, fetStateStr)
    let paStateStr = B64.encode . encode $ initialPendingActionsState
    execute conn
        "insert into states values (?, ?)"
        ("pendingactionsstate" :: B.ByteString, paStateStr)
    execute_ conn
        "create table accounts ( account text primary key\
                               \, btc_balance integer\
                               \, usd_balance integer)"
    return ()
