{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module ClientHub
    ( initClientHub
    , ClientHubHandle
    , registerClientWithHub
    , requestClientStatus
    , signalPossibleBitcoinEvents
    , signalAccountUpdates
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Database.PostgreSQL.Simple
import Data.Foldable(foldl',foldlM)
import Data.IxSet((@<), (@>=), (@=))
import Data.Time.Clock
import Data.Typeable

import qualified Data.IxSet as I
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC
import qualified Network.BitcoinRPC.Events.MarkerAddresses as MA

import AddressUtils
import CommonTypes
import Config
import DbUtils
import LoggingUtils

magicAddress = RPC.BitcoinAddress "1KmWJbRo4sjcQBFZN83KExVjBxTNxST6fL"

timeoutInSeconds :: Int
timeoutInSeconds = 5 --25

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

type AddressCache = M.Map RPC.BitcoinAddress (Maybe BridgewalkerAccount)

type AccountCache = M.Map BridgewalkerAccount [ClientPendingTransaction]

initClientHub :: BridgewalkerHandles -> IO ClientHubHandle
initClientHub bwHandles = do
    handle <- ClientHubHandle <$> newChan
    _ <- forkIO $ clientHubLoop handle bwHandles
    _ <- forkIO $ periodicTimeoutCheck handle
    return handle

clientHubLoop ::  ClientHubHandle -> BridgewalkerHandles -> IO ()
clientHubLoop (ClientHubHandle chChan) bwHandles = do
    (accountCache, addressCache) <-
        updatePendingInfos bwHandles M.empty
    go I.empty addressCache accountCache
  where
    go clientSet addressCache accountCache = do
        let logger = bhAppLogger bwHandles
        msg <- readChan chChan
        case msg of
            RegisterClient account answerChan -> do
                clientSet' <- addClient clientSet account answerChan
                logger $ UserLoggedIn (bAccount account)
                go clientSet' addressCache accountCache
            RequestClientStatus account -> do
                case I.getOne (clientSet @= account) of
                    Nothing -> go clientSet addressCache accountCache
                    Just client -> do
                        sendClientStatus bwHandles accountCache client
                        go clientSet addressCache accountCache
            ReceivedPing account -> do
                case I.getOne (clientSet @= account) of
                    Nothing -> go clientSet addressCache accountCache
                    Just client -> do
                        now <- getCurrentTime
                        let client' = client { cdLastKeepAlive = now }
                            clientSet' = I.updateIx account client' clientSet
                        sendPong client
                        go clientSet' addressCache accountCache
            SignalPossibleBitcoinEvents -> do
                (accountCache', addressCache') <-
                    updatePendingInfos bwHandles addressCache
                let affectedClients =
                        findAffectedClients (I.toList clientSet) accountCache
                                                                   accountCache'
                mapM_ (sendClientStatus bwHandles accountCache') affectedClients
                go clientSet addressCache' accountCache'
            SignalAccountUpdates accounts -> do
                forM accounts $ \account -> do
                    case I.getOne (clientSet @= account) of
                        Nothing -> return ()
                        Just client ->
                            sendClientStatus bwHandles accountCache client
                go clientSet addressCache accountCache
            CheckTimeouts -> do
                now <- getCurrentTime
                let cutoff = addUTCTime (-1 * fromIntegral timeoutInSeconds) now
                -- do a fast check first, before actually
                -- touching the datastructure
                let staleClients = clientSet @< cutoff
                if I.null staleClients
                    then go clientSet addressCache accountCache
                    else do
                        mapM_ closeConnection $ I.toList staleClients
                        go (clientSet @>= cutoff) addressCache accountCache

periodicTimeoutCheck :: ClientHubHandle -> IO b
periodicTimeoutCheck (ClientHubHandle chChan) =
    forever $ do
        threadDelay $ timeoutInSeconds * 1000 * 1000
        writeChan chChan CheckTimeouts

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

receivedPing :: ClientHubHandle -> BridgewalkerAccount -> IO ()
receivedPing (ClientHubHandle chChan) account = do
    writeChan chChan $ ReceivedPing account
    return ()

signalPossibleBitcoinEvents :: ClientHubHandle -> IO ()
signalPossibleBitcoinEvents (ClientHubHandle chChan) = do
    writeChan chChan $ SignalPossibleBitcoinEvents
    return ()

signalAccountUpdates :: ClientHubHandle -> [BridgewalkerAccount] -> IO ()
signalAccountUpdates (ClientHubHandle chChan) accounts = do
    writeChan chChan $ SignalAccountUpdates accounts
    return ()

sendClientStatus :: BridgewalkerHandles-> AccountCache-> ClientData-> IO ()
sendClientStatus bwHandles accountCache client = do
    let dbConn = bhDBConnCH bwHandles
        account = cdAccount client
        answerChan = cdAnswerChan client
    (btcIn, usdBalance, primaryBTCAddress) <-
        getClientDBStatus dbConn (bAccount account)
    let pendingTxs = case M.lookup account accountCache of
                        Just vs -> vs
                        Nothing -> []
    let status = ClientStatus { csUSDBalance = usdBalance
                              , csBTCIn = btcIn
                              , csPrimaryBTCAddress = primaryBTCAddress
                              , csPendingTxs = pendingTxs
                              }
    writeChan answerChan $ ForwardStatusToClient status

sendPong :: ClientData -> IO ()
sendPong client = writeChan (cdAnswerChan client) SendPongToClient

closeConnection :: ClientData -> IO ()
closeConnection client =
    writeChan (cdAnswerChan client) CloseConnectionWithClient

findAffectedClients :: [ClientData]-> AccountCache -> AccountCache -> [ClientData]
findAffectedClients clients oldAccountCache newAccountCache =
    concatMap go clients
  where
    go client =
        let account = cdAccount client
        in if M.lookup account oldAccountCache == M.lookup account newAccountCache
            then []
            else [client]

updatePendingInfos :: BridgewalkerHandles-> AddressCache -> IO (AccountCache, AddressCache)
updatePendingInfos bwHandles addressCache = do
    let dbConn = bhDBConnCH bwHandles
        fetStateCopy = bhFilteredEventStateCopy bwHandles
    fetState <- readMVar fetStateCopy
    let pendingTxs = MA.listPendingTransactions fetState
    (augPendingTxs, updatedAddressCache) <-
        foldlM (augmentPendingTxs dbConn addressCache) ([], M.empty) pendingTxs
    let updatedAccountCache = foldl' rebuildAccountCache M.empty augPendingTxs
    return (updatedAccountCache, updatedAddressCache)

augmentPendingTxs :: Connection-> AddressCache -> ([(Maybe BridgewalkerAccount, (RPC.Transaction, b))],M.Map RPC.BitcoinAddress (Maybe BridgewalkerAccount))-> (RPC.Transaction, b)-> IO([(Maybe BridgewalkerAccount, (RPC.Transaction, b))],AddressCache)
augmentPendingTxs dbConn addressCache (augPendingTxs, updatedAddressCache) pendingTx = do
    let addr = RPC.tAddress . fst $ pendingTx
    case M.lookup addr addressCache of
        Just v -> return $ ((v, pendingTx) : augPendingTxs,
                                M.insert addr v updatedAddressCache)
        Nothing -> do
            mAccount <- getAccountByAddress dbConn (adjustAddr addr)
            return $ ((mAccount, pendingTx) : augPendingTxs,
                        M.insert addr mAccount updatedAddressCache)

rebuildAccountCache :: AccountCache -> (Maybe BridgewalkerAccount, (RPC.Transaction, MA.PendingReason))-> AccountCache
rebuildAccountCache accountCache augPendingTx =
    let (mAccount, (tx, reason)) = augPendingTx
    in case mAccount of
        Nothing -> accountCache
        Just account ->
            let tPendingTx = translatePendingTx tx reason
            in case M.lookup account accountCache of
                Nothing -> M.insert account [tPendingTx] accountCache
                Just vs -> M.insert account (tPendingTx:vs) accountCache
  where
    translatePendingTx tx reason =
        let amount = RPC.btcAmount . RPC.tAmount $ tx
            cReason = case reason of
                        MA.TooFewConfirmations confs ->
                            TooFewConfirmations confs
                        MA.MarkerAddressLimitReached markerAddress ->
                            MarkerAddressLimitReached (RPC.btcAddress
                                                            markerAddress)
        in ClientPendingTransaction amount cReason
