{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module ClientHub
    ( compileClientStatus
    , initClientHub
    , ClientHubHandle
    , registerClientWithHub
    , requestClientStatus
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.IxSet((@<), (@>=), (@=))
import Data.Time.Clock
import Data.Typeable

import qualified Data.IxSet as I
import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC
import qualified Network.BitcoinRPC.Events.MarkerAddresses as MA

import CommonTypes
import Config
import DbUtils

magicAddress = RPC.BitcoinAddress "1KmWJbRo4sjcQBFZN83KExVjBxTNxST6fL"

data ClientData = ClientData { cdAccount :: BridgewalkerAccount
                             , cdLastKeepAlive :: UTCTime
                             , cdAnswerChan :: Chan ClientHubAnswer
                             }
                  deriving (Eq, Typeable)

instance Ord ClientData where
    (ClientData acc keepAlive _) `compare` (ClientData acc' keepAlive' _) =
        case acc `compare` acc' of
            LT -> LT
            GT -> GT
            EQ -> keepAlive `compare` keepAlive'

instance I.Indexable ClientData where
    empty = I.ixSet [ I.ixFun $ \e -> [ cdAccount e ]
                    , I.ixFun $ \e -> [ cdLastKeepAlive e ]
                    ]

initClientHub :: BridgewalkerHandles -> IO ClientHubHandle
initClientHub bwHandles = do
    handle <- ClientHubHandle <$> newChan
    _ <- forkIO $ clientHubLoop handle bwHandles
    return handle

clientHubLoop ::  ClientHubHandle -> BridgewalkerHandles -> IO ()
clientHubLoop (ClientHubHandle chChan) bwHandles = go I.empty
  where
    go clientSet = do
        msg <- readChan chChan
        case msg of
            RegisterClient account answerChan -> do
                clientSet' <- addClient clientSet account answerChan
                go clientSet'
            RequestClientStatus account -> do
                case I.getOne (clientSet @= account) of
                    Nothing -> go clientSet
                    Just (cd@ClientData{}) -> do
                        status <- compileClientStatus bwHandles account
                        writeChan (cdAnswerChan cd) $ ForwardStatusToClient status
                        go clientSet

-- TODO: remove stale clients from time to time

addClient :: I.IxSet ClientData-> BridgewalkerAccount-> Chan ClientHubAnswer-> IO (I.IxSet ClientData)
addClient set account answerChan = do
    now <- getCurrentTime
    return $ I.updateIx account (ClientData account now answerChan) set

registerClientWithHub :: ClientHubHandle -> BridgewalkerAccount -> IO (Chan ClientHubAnswer)
registerClientWithHub (ClientHubHandle chChan) account = do
    answerChan <- newChan
    writeChan chChan $ RegisterClient { chcAccount = account
                                      , chcAnswerChan = answerChan
                                      }
    return answerChan

requestClientStatus :: ClientHubHandle -> BridgewalkerAccount -> IO ()
requestClientStatus (ClientHubHandle chChan) account = do
    writeChan chChan $ RequestClientStatus account
    return ()

compileClientStatus :: BridgewalkerHandles -> BridgewalkerAccount -> IO ClientStatus
compileClientStatus bwHandles bwAccount = do
    let dbConn = bhDBConnCH bwHandles
        fetStateCopy = bhFilteredEventStateCopy bwHandles
        account = bAccount bwAccount
    (btcIn, usdBalance, primaryBTCAddress) <- getClientDBStatus dbConn account
    fetState <- readMVar fetStateCopy
    let pendingTxs = map translatePendingTx
                        . filter ((==) magicAddress . RPC.tAddress . fst)
                        . MA.listPendingTransactions $ fetState
    let status = ClientStatus { csUSDBalance = usdBalance
                              , csBTCIn = btcIn
                              , csPrimaryBTCAddress = primaryBTCAddress
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
