{-# LANGUAGE OverloadedStrings #-}
module Tools.CreateDB where

import Database.PostgreSQL.Simple

import qualified Data.ByteString as B

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

createTableSmallTxFund :: Connection -> IO ()
createTableSmallTxFund conn = do
    _ <- execute_ conn
            "create table small_tx_fund\
                \( id serial primary key\
                \, timestamp timestamp with time zone\
                \, description text\
                \, btc_change integer\
                \, usd_change integer\
                \, btc_total integer\
                \, usd_total integer\
                \)"
    _ <- execute_ conn "create index small_tx_fund_timestamp_idx\
                       \ on small_tx_fund (timestamp)"
    _ <- execute conn "insert into small_tx_fund (timestamp, description\
                            \, btc_change, usd_change, btc_total, usd_total)\
                            \ values (?, ?, 0, 0, 0, 0)"
                            ("Now" :: String,
                                "Initialized small tx fund." :: String)
    return ()


main :: IO ()
main = connectPostgreSQL myConnectInfo >>= \conn -> do
    createTableSmallTxFund conn
    return ()
