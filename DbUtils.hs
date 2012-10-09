{-# LANGUAGE OverloadedStrings #-}
module DbUtils
    ( readBitcoindStateFromDB
    , writeBitcoindStateToDB
    , readPendingActionsStateFromDB
    , writePendingActionsStateToDB
    , getBTCInBalance
    , getUSDBalance
    , getClientDBStatus
    ) where

import Control.Applicative
import Database.PostgreSQL.Simple
import Data.Serialize
import Network.BitcoinRPC.Events.MarkerAddresses

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import CommonTypes

readStateFromDB :: Connection -> B.ByteString -> IO B.ByteString
readStateFromDB conn key = do
    Only stateStr <- getFirstRow <$>
        query conn "select state from states where key=?" (Only key)
    return stateStr
  where
    getFirstRow [] = error "Expected at least one row, but got none."
    getFirstRow (x:_) = x

writeStateToDB :: Connection -> B.ByteString -> B.ByteString -> IO ()
writeStateToDB conn key stateStr = do
    execute conn "update states set state=? where key=?" (stateStr, key)
    return ()

writeBitcoindStateToDB :: Connection -> FilteredEventTaskState -> IO ()
writeBitcoindStateToDB conn fetState = do
    let fetStateStr = B64.encode . encode $ fetState
    writeStateToDB conn "filteredeventtaskstate" fetStateStr
    return ()

readBitcoindStateFromDB :: Connection -> IO (FilteredEventTaskState)
readBitcoindStateFromDB conn = do
    fetStateStr <- readStateFromDB conn "filteredeventtaskstate"
    let fetState = B64.decode fetStateStr >>= decode
    return $ expectRight fetState

writePendingActionsStateToDB :: Connection -> PendingActionsState -> IO ()
writePendingActionsStateToDB conn paState = do
    let paStateStr = B64.encode . encode $ paState
    writeStateToDB conn "pendingactionsstate" paStateStr
    return ()

readPendingActionsStateFromDB :: Connection -> IO (PendingActionsState)
readPendingActionsStateFromDB conn = do
    paStateStr <- readStateFromDB conn "pendingactionsstate"
    let paState = B64.decode paStateStr >>= decode
    return $ expectRight paState

expectRight :: Either String t -> t
expectRight (Right r) = r
expectRight (Left msg) = error msg

getBTCInBalance :: Connection -> Integer -> IO Integer
getBTCInBalance dbConn account = do
    let errMsg = "Expected to find account " ++ show account
                    ++ " while doing getBTCInBalance, but failed."
    Only balance <- expectOneRow errMsg <$>
        query dbConn "select btc_in from accounts where account_nr=?"
                        (Only account)
    return balance

getUSDBalance :: Connection -> Integer -> IO Integer
getUSDBalance dbConn account = do
    let errMsg = "Expected to find account " ++ show account
                    ++ " while doing getUSDBalance, but failed."
    Only balance <- expectOneRow errMsg <$>
        query dbConn "select usd_balance from accounts where account_nr=?"
                        (Only account)
    return balance

getClientDBStatus :: Connection -> Integer -> IO (Integer, Integer)
getClientDBStatus dbConn account = do
    let errMsg = "Expected to find account " ++ show account
                    ++ " while doing getClientDBStatus, but failed."
    (btcIn, usdBalance) <- expectOneRow errMsg <$>
        query dbConn "select btc_in, usd_balance from accounts\
                        \ where account_nr=?" (Only account)
    return (btcIn, usdBalance)

expectOneRow :: String -> [a] -> a
expectOneRow errMsg [] = error errMsg
expectOneRow _ (x:_) = x
