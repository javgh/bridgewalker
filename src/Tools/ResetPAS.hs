{-# LANGUAGE OverloadedStrings #-}
module Tools.CreateDB where

import Control.Monad
import Database.PostgreSQL.Simple
import Data.Serialize

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import PendingActionsTracker

myConnectInfo :: B.ByteString
myConnectInfo = "dbname=bridgewalker"

resetPendingActionsState :: Connection -> IO ()
resetPendingActionsState conn =
    let paStateStr = B64.encode . encode $ initialPendingActionsState
    in void $ execute conn
                "update states set state=? where key=?"
                (paStateStr, "pendingactionsstate" :: B.ByteString)

main :: IO ()
main = connectPostgreSQL myConnectInfo >>= \conn -> do
    resetPendingActionsState conn
    return ()
