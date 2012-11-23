{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Network.BitcoinRPC.Events.MarkerAddresses
import Data.Serialize

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import PendingActionsTracker

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

createTableStates :: Connection -> IO ()
createTableStates conn = do
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
    return ()

createTableAccounts :: Connection -> IO ()
createTableAccounts conn = do
    execute_ conn
        "create table accounts ( account_nr serial primary key\
                               \, btc_in integer\
                               \, usd_balance integer\
                               \, account_name text\
                               \, account_pw text\
                               \, is_full_account boolean\
                               \, primary_btc_address text\
                               \)"
    execute_ conn
        "create index accounts_account_name_index on accounts (account_name)"
    return ()

createTableAddresses :: Connection -> IO ()
createTableAddresses conn = do
    execute_ conn
        "create table addresses ( id serial primary key\
                               \, account integer\
                               \            references accounts(account_nr)\
                               \, btc_address text unique\
                               \)"
    execute_ conn
        "create index addresses_btc_address_idx on addresses (btc_address)"
    return ()


main :: IO ()
main = connectPostgreSQL myConnectInfo >>= \conn -> do
    --createTableStates conn
    createTableAccounts conn
    createTableAddresses conn
    return ()
