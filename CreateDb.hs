{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Network.BitcoinRPC.Events.MarkerAddresses
import Data.Serialize

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

main :: IO ()
main = connectPostgreSQL myConnectInfo >>= \conn -> do
    execute_ conn
        "create table bitcoind (key text primary key, state text)"
    let fetStateStr = B64.encode . encode $ initialFilteredEventTaskState
    execute conn
        "insert into bitcoind values (?, ?)"
        ("filteredeventtaskstate" :: B.ByteString, fetStateStr)
    return ()
