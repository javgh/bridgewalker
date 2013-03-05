{-# LANGUAGE OverloadedStrings #-}
module DbUtils
    ( readBitcoindStateFromDB
    , writeBitcoindStateToDB
    , readPendingActionsStateFromDB
    , writePendingActionsStateToDB
    , getBTCInBalance
    , getUSDBalance
    , getClientDBStatus
    , checkGuestNameExists
    , checkLogin
    , getAccountNumber
    , getAccountByAddress
    , debugConnection
    , withSerialTransaction
    ) where

import Control.Applicative
import Control.Concurrent
import Crypto.PasswordStore
import Database.PostgreSQL.Simple
import Data.Serialize
import Network.BitcoinRPC.Events.MarkerAddresses

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import CommonTypes

debugConnection :: IO Connection
debugConnection = connectPostgreSQL "dbname=bridgewalker"

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
    _ <- execute conn "update states set state=? where key=?" (stateStr, key)
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

getClientDBStatus :: Connection -> Integer -> IO (Integer, Integer, T.Text)
getClientDBStatus dbConn account = do
    let errMsg = "Expected to find account " ++ show account
                    ++ " while doing getClientDBStatus, but failed."
    status <- expectOneRow errMsg <$>
        query dbConn "select btc_in, usd_balance, primary_btc_address\
                        \ from accounts where account_nr=?" (Only account)
    return status

checkGuestNameExists :: Connection -> T.Text -> IO Bool
checkGuestNameExists dbConn guestName = do
    result <- query dbConn "select 1 from accounts where account_name=?"
                                (Only guestName) :: IO [Only Integer]
    return $ length result > 0

checkLogin :: Connection -> T.Text -> T.Text -> IO (Maybe BridgewalkerAccount)
checkLogin dbConn accountName accountPassword = do
    result <- query dbConn "select account_nr, account_pw from accounts\
                                \ where account_name=?" (Only accountName)
    case result of
        [] -> return Nothing
        ((accountNr, pwHash):_) ->
            if verifyPassword (T.encodeUtf8 accountPassword) pwHash
                then return $ Just (BridgewalkerAccount accountNr)
                else return Nothing

getAccountNumber :: Connection -> T.Text -> IO Integer
getAccountNumber dbConn accountName = do
    let errMsg = "Expected to find account " ++ T.unpack accountName
                    ++ " while doing getAccountNumber, but failed."
    (Only account) <- expectOneRow errMsg <$>
        query dbConn "select account_nr from accounts\
                        \ where account_name=?" (Only accountName)
    return account

getAccountByAddress :: Connection -> T.Text -> IO (Maybe BridgewalkerAccount)
getAccountByAddress dbConn btcAddress = do
    result <- query dbConn "select account from addresses\
                                \ where btc_address=?" (Only btcAddress)
    case result of
        [] -> return Nothing
        (Only accountNr:_) -> return $ Just (BridgewalkerAccount accountNr)

expectOneRow :: String -> [a] -> a
expectOneRow errMsg [] = error errMsg
expectOneRow _ (x:_) = x

withSerialTransaction ::  MVar () -> Connection -> IO b -> IO b
withSerialTransaction dbWriteLock dbConn action = do
    _ <- takeMVar dbWriteLock
    r <- withTransaction dbConn action
    putMVar dbWriteLock ()
    return r
