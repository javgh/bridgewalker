{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Network.BitcoinRPC.Events
import Network.BitcoinRPC.MarkerAddresses
import Data.Serialize

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

main :: IO ()
main = connectPostgreSQL myConnectInfo >>= \conn -> do
    execute_ conn
        "create table bitcoind (key text primary key, state text)"
    let etStateStr = B64.encode . encode $ initialEventTaskState
    let maStoreStr = B64.encode . encode $ initMarkerAddressStore []
    execute conn
        "insert into bitcoind values (?, ?)"
        ("eventtaskstate" :: B.ByteString, etStateStr)
    execute conn
        "insert into bitcoind values (?, ?)"
        ("mastore" :: B.ByteString, maStoreStr)
    return ()
