{-# LANGUAGE OverloadedStrings #-}
module Tools.CreateDB where

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
    _ <- execute_ conn
            "create table states (key text primary key, state text)"
    initializeFilteredEventTaskState conn
    initializePendingActionsState conn
    return ()

initializeFilteredEventTaskState :: Connection -> IO ()
initializeFilteredEventTaskState conn =
    let fetStateStr = B64.encode . encode $ initialFilteredEventTaskState
    in execute conn
        "insert into states values (?, ?)"
        ("filteredeventtaskstate" :: B.ByteString, fetStateStr) >> return ()

initializePendingActionsState :: Connection -> IO ()
initializePendingActionsState conn =
    let paStateStr = B64.encode . encode $ initialPendingActionsState
    in execute conn
        "insert into states values (?, ?)"
        ("pendingactionsstate" :: B.ByteString, paStateStr) >> return ()

resetPendingActionsState :: Connection -> IO ()
resetPendingActionsState conn =
    let paStateStr = B64.encode . encode $ initialPendingActionsState
    in execute conn
        "update states set state=? where key=?"
        (paStateStr, "pendingactionsstate" :: B.ByteString) >> return ()

createTableAccounts :: Connection -> IO ()
createTableAccounts conn = do
    _ <- execute_ conn
            "create table accounts ( account_nr serial primary key\
                                   \, btc_in integer\
                                   \, usd_balance integer\
                                   \, account_name text\
                                   \, account_pw text\
                                   \, is_full_account boolean\
                                   \, primary_btc_address text\
                                   \)"
    _ <- execute_ conn "create index accounts_account_name_index\
                       \ on accounts (account_name)"
    return ()

createTableAddresses :: Connection -> IO ()
createTableAddresses conn = do
    _ <- execute_ conn
            "create table addresses ( id serial primary key\
                                   \, account integer\
                                   \            references accounts(account_nr)\
                                   \, btc_address text unique\
                                   \)"
    _ <- execute_ conn "create index addresses_btc_address_idx\
                       \ on addresses (btc_address)"
    return ()


main :: IO ()
main = connectPostgreSQL myConnectInfo >>= \conn -> do
    createTableStates conn
    createTableAccounts conn
    createTableAddresses conn
    return ()
