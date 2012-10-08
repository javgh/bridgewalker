{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.PostgreSQL.Simple

import qualified Data.ByteString as B
import qualified Network.BitcoinRPC as RPC

import DbUtils
import PendingActionsTracker

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

addBuyAction dbConn = withTransaction dbConn $ do
    paState <- readPendingActionsStateFromDB dbConn
    let action = BuyBTCAction
                    { baAmount = 1000000
                    , baAddress =
                        RPC.BitcoinAddress "1AvcR6BFmnd8SnZiJfDm6rrQ2aTkdHsf6N"
                    , baAccount = BridgewalkerAccount 1
                    }
    let paState' = addPendingActions paState [action]
    writePendingActionsStateToDB dbConn paState'

main = do
    dbConn <- connectPostgreSQL myConnectInfo
    addBuyAction dbConn
