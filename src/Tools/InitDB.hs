{-# LANGUAGE OverloadedStrings #-}
module Tools.InitDB
    ( initDB
    , resetFilteredEventTaskState
    ) where

import Control.Monad
import Database.PostgreSQL.Simple
import Data.Serialize
import Network.BitcoinRPC.Events.MarkerAddresses

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
    in void $ execute conn
                "insert into states values (?, ?)"
                ("filteredeventtaskstate" :: B.ByteString, fetStateStr)

initializePendingActionsState :: Connection -> IO ()
initializePendingActionsState conn =
    let paStateStr = B64.encode . encode $ initialPendingActionsState
    in void $ execute conn
                "insert into states values (?, ?)"
                ("pendingactionsstate" :: B.ByteString, paStateStr)

createTableAccounts :: Connection -> IO ()
createTableAccounts conn = do
    _ <- execute_ conn
            "create table accounts ( account_nr serial primary key\
                                   \, btc_in bigint\
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

initDB :: IO ()
initDB = connectPostgreSQL myConnectInfo >>= \conn -> do
    createTableStates conn
    createTableAccounts conn
    createTableAddresses conn
    return ()

resetFilteredEventTaskState :: IO ()
resetFilteredEventTaskState =
    connectPostgreSQL myConnectInfo >>= \conn ->
        let fetStateStr = B64.encode . encode $ initialFilteredEventTaskState
        in void $ execute conn
                    "update states set state=? where key=?"
                    (fetStateStr, "filteredeventtaskstate" :: B.ByteString)
