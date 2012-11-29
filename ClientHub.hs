{-# LANGUAGE OverloadedStrings #-}
module ClientHub
    ( compileClientStatus
    , initClientHub
    , ClientHubHandle
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Aeson

import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC
import qualified Network.BitcoinRPC.Events.MarkerAddresses as MA

import CommonTypes
import Config
import DbUtils

magicAddress = RPC.BitcoinAddress "17cWnmBb4b8EMrHhSiasMXsbsc1ru7iTGj"

-- TODO: switch from accountName to accountNumber (BridgewalkerAccount)

--initClientHub :: IO ClientHubHandle
initClientHub bwHandles = do
    handle <- ClientHubHandle <$> newChan
    _ <- forkIO $ clientHubLoop handle bwHandles
    return handle

--clientHubLoop :: ClientHubHandle -> IO ()
clientHubLoop (ClientHubHandle chChan) bwHandles = go []
  where
    go clientList = do
        msg <- readChan chChan
        case msg of
            RegisterClient accountName answerChan ->
                let clientList' = (accountName, answerChan) : clientList
                in go clientList'

registerClientWithHub :: ClientHubHandle -> T.Text -> IO (Chan ClientHubAnswer)
registerClientWithHub (ClientHubHandle chChan) accountName = do
    answerChan <- newChan
    writeChan chChan $ RegisterClient { chcAccountName = accountName
                                      , chcAnswerChan = answerChan
                                      }
    return answerChan

requestClientStatus :: ClientHubHandle -> T.Text -> IO ()
requestClientStatus (ClientHubHandle chChan) accountName = do
    writeChan chChan $ RequestClientStatus accountName
    return ()

compileClientStatus :: BridgewalkerHandles -> BridgewalkerAccount -> IO ClientStatus
compileClientStatus bwHandles bwAccount = do
    let dbConn = bhDBConnCH bwHandles
        fetStateCopy = bhFilteredEventStateCopy bwHandles
        account = bAccount bwAccount
    (btcIn, usdBalance) <- getClientDBStatus dbConn account
    fetState <- readMVar fetStateCopy
    let pendingTxs = map translatePendingTx
                        . filter ((==) magicAddress . RPC.tAddress . fst)
                        . MA.listPendingTransactions $ fetState
    let status = ClientStatus { csUSDBalance = usdBalance
                              , csBTCIn = btcIn
                              , csPendingTxs = pendingTxs
                              }
    return status
  where
    translatePendingTx (tx, reason) =
        let amount = RPC.btcAmount . RPC.tAmount $ tx
            cReason = case reason of
                        MA.TooFewConfirmations confs ->
                            TooFewConfirmations confs
                        MA.MarkerAddressLimitReached markerAddress ->
                            MarkerAddressLimitReached (RPC.btcAddress
                                                            markerAddress)
        in ClientPendingTransaction amount cReason
