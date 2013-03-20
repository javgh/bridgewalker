{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module ClientHub
    ( initClientHub
    , ClientHubHandle
    , registerClientWithHub
    , requestClientStatus
    , requestQuote
    , sendPayment
    , signalPossibleBitcoinEvents
    , signalAccountUpdates
    , signalSuccessfulSend
    , signalFailedSend
    , receivedPing
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Database.PostgreSQL.Simple
import Data.Foldable(foldl',foldlM)
import Data.IxSet((@<), (@>=), (@=))
import Data.Maybe
import Data.Time.Clock
import Data.Typeable
import Network.MtGoxAPI

import qualified Data.IxSet as I
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC
import qualified Network.BitcoinRPC.Events.MarkerAddresses as MA

import AddressUtils
import CommonTypes
import Config
import DbUtils
import PendingActionsTrackerQueueManagement
import QuoteUtils

timeoutInSeconds :: Int
timeoutInSeconds = 30

exchangeCheckInterval :: Int
exchangeCheckInterval = 60  -- seconds

sendPaymentInterval :: NominalDiffTime
sendPaymentInterval = 10    -- SendPayment commands have to be executed
                            -- within 10 seconds or not at all

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
    _ <- forkIO $ periodicExchangeCheck handle bwHandles
    return handle

clientHubLoop ::  ClientHubHandle -> BridgewalkerHandles -> IO ()
clientHubLoop (ClientHubHandle chChan) bwHandles = do
    (accountCache, addressCache) <-
        updatePendingInfos bwHandles M.empty
    go I.empty addressCache accountCache ExchangeUnavailable
  where
    go clientSet addressCache accountCache exchangeStatus = do
        let logger = bhAppLogger bwHandles
        msg <- readChan chChan
        case msg of
            RegisterClient account answerChan -> do
                clientSet' <- addClient clientSet account answerChan
                logger $ UserLoggedIn (bAccount account)
                go clientSet' addressCache accountCache exchangeStatus
            RequestClientStatus account -> do
                case I.getOne (clientSet @= account) of
                    Nothing -> return ()
                    Just client ->
                        sendClientStatus bwHandles
                            accountCache exchangeStatus client
                go clientSet addressCache accountCache exchangeStatus
            RequestQuote account requestID amountType -> do
                case I.getOne (clientSet @= account) of
                    Nothing -> return ()
                    Just client ->
                        sendQuote bwHandles client account requestID amountType
                go clientSet addressCache accountCache exchangeStatus
            SendPayment account requestID address amountType -> do
                sendPaymentToPAT bwHandles account requestID address amountType
                go clientSet addressCache accountCache exchangeStatus
            ReceivedPing account ->
                case I.getOne (clientSet @= account) of
                    Nothing -> go clientSet addressCache
                                    accountCache exchangeStatus
                    Just client -> do
                        now <- getCurrentTime
                        let client' = client { cdLastKeepAlive = now }
                            clientSet' = I.updateIx account client' clientSet
                        sendPong client
                        go clientSet' addressCache
                                accountCache exchangeStatus
            SignalPossibleBitcoinEvents -> do
                (accountCache', addressCache') <-
                    updatePendingInfos bwHandles addressCache
                let affectedClients =
                        findAffectedClients (I.toList clientSet) accountCache
                                                                   accountCache'
                mapM_ (sendClientStatus bwHandles
                            accountCache' exchangeStatus) affectedClients
                go clientSet addressCache' accountCache' exchangeStatus
            SignalAccountUpdates accounts -> do
                forM_ accounts $ \account ->
                    case I.getOne (clientSet @= account) of
                        Nothing -> return ()
                        Just client ->
                            sendClientStatus bwHandles
                                    accountCache exchangeStatus client
                go clientSet addressCache accountCache exchangeStatus
            SignalFailedSend account requestID reason -> do
                case I.getOne (clientSet @= account) of
                    Nothing -> return ()
                    Just client -> sendFailedSend client requestID reason
                go clientSet addressCache accountCache exchangeStatus
            SignalSuccessfulSend account requestID -> do
                case I.getOne (clientSet @= account) of
                    Nothing -> return ()
                    Just client -> sendSuccessfulSend client requestID
                go clientSet addressCache accountCache exchangeStatus
            CheckTimeouts -> do
                now <- getCurrentTime
                let cutoff = addUTCTime (-1 * fromIntegral timeoutInSeconds) now
                -- do a fast check first, before actually
                -- touching the datastructure
                let staleClients = clientSet @< cutoff
                if I.null staleClients
                    then go clientSet addressCache
                                accountCache exchangeStatus
                    else do
                        mapM_ closeConnection $ I.toList staleClients
                        let clientSet' = clientSet @>= cutoff
                            staleCount = I.size staleClients
                            connectedCount = I.size clientSet'
                            logMsg =
                                DisconnectedStaleClient
                                    { lcClientsDisconnected =
                                        fromIntegral staleCount
                                    , lcClientsRemaining =
                                        fromIntegral connectedCount
                                    , lcInfo = "Disconnected "
                                        ++ show staleCount ++ " stale\
                                        \ client(s); " ++ show connectedCount
                                        ++ " client(s) remaining online."
                                    }
                        logger logMsg
                        go clientSet' addressCache
                                accountCache exchangeStatus
            ExchangeUpdate exchangeStatus' -> do
                case (exchangeStatus, exchangeStatus') of
                    (ExchangeUnavailable, ExchangeAvailable _) ->
                        -- exchange has just become available;
                        -- inform all clients
                        forM_ (I.toList clientSet) $
                            sendClientStatus bwHandles
                                accountCache exchangeStatus'
                    (ExchangeAvailable _, ExchangeUnavailable) ->
                        -- exchange has just become unavailable;
                        -- inform all clients
                        forM_ (I.toList clientSet) $
                            sendClientStatus bwHandles
                                accountCache exchangeStatus'
                    _ -> return ()  -- no change in exchange status;
                                    -- do not do extra updates for rate changes
                go clientSet addressCache accountCache exchangeStatus'

periodicTimeoutCheck :: ClientHubHandle -> IO ()
periodicTimeoutCheck (ClientHubHandle chChan) =
    forever $ do
        threadDelay $ timeoutInSeconds * 1000 * 1000
        writeChan chChan CheckTimeouts

periodicExchangeCheck :: ClientHubHandle -> BridgewalkerHandles -> IO ()
periodicExchangeCheck (ClientHubHandle chChan) bwHandles =
    let depthStoreHandle = mtgoxDepthStoreHandle . bhMtGoxHandles $ bwHandles
        testAmount = 1 * 10 ^ (8 :: Integer)
    in forever $ do
        testBuy <- simulateBTCBuy depthStoreHandle testAmount
        testSell <- case testBuy of
                        DepthStoreAnswer _ ->
                            simulateBTCSell depthStoreHandle testAmount
                        _ -> return DepthStoreUnavailable   -- short circuit
        let status = case (testBuy, testSell) of
                        (DepthStoreAnswer buyRate, DepthStoreAnswer sellRate) ->
                            ExchangeAvailable $ (buyRate + sellRate) `div` 2
                        _ -> ExchangeUnavailable
        writeChan chChan $ ExchangeUpdate status
        threadDelay $ exchangeCheckInterval * 1000 * 1000

addClient :: I.IxSet ClientData-> BridgewalkerAccount-> Chan ClientHubAnswer-> IO (I.IxSet ClientData)
addClient set account answerChan = do
    now <- getCurrentTime
    return $ I.updateIx account (ClientData account now answerChan) set

registerClientWithHub :: ClientHubHandle -> BridgewalkerAccount -> IO (Chan ClientHubAnswer)
registerClientWithHub (ClientHubHandle chChan) account = do
    answerChan <- newChan
    writeChan chChan RegisterClient { chcAccount = account
                                    , chcAnswerChan = answerChan
                                    }
    return answerChan

requestClientStatus :: ClientHubHandle -> BridgewalkerAccount -> IO ()
requestClientStatus (ClientHubHandle chChan) account = do
    writeChan chChan $ RequestClientStatus account
    return ()

requestQuote :: ClientHubHandle -> BridgewalkerAccount -> Integer -> AmountType -> IO ()
requestQuote (ClientHubHandle chChan) account requestID amountType = do
    writeChan chChan $ RequestQuote account requestID amountType
    return ()

sendPayment :: ClientHubHandle -> BridgewalkerAccount -> Integer -> T.Text -> AmountType -> IO ()
sendPayment (ClientHubHandle chChan) account requestID address amountType =
    writeChan chChan $ SendPayment account requestID address amountType

receivedPing :: ClientHubHandle -> BridgewalkerAccount -> IO ()
receivedPing (ClientHubHandle chChan) account = do
    writeChan chChan $ ReceivedPing account
    return ()

signalPossibleBitcoinEvents :: ClientHubHandle -> IO ()
signalPossibleBitcoinEvents (ClientHubHandle chChan) = do
    writeChan chChan SignalPossibleBitcoinEvents
    return ()

signalAccountUpdates :: ClientHubHandle -> [BridgewalkerAccount] -> IO ()
signalAccountUpdates (ClientHubHandle chChan) accounts = do
    writeChan chChan $ SignalAccountUpdates accounts
    return ()

signalSuccessfulSend :: ClientHubHandle -> BridgewalkerAccount -> Integer -> IO ()
signalSuccessfulSend (ClientHubHandle chChan) account requestID = do
    writeChan chChan $ SignalSuccessfulSend account requestID
    return ()

signalFailedSend :: ClientHubHandle-> BridgewalkerAccount -> Integer -> T.Text -> IO ()
signalFailedSend (ClientHubHandle chChan) account requestID reason = do
    writeChan chChan $ SignalFailedSend account requestID reason
    return ()

sendPaymentToPAT :: IsBitcoinAddress a =>BridgewalkerHandles-> BridgewalkerAccount -> Integer -> a -> AmountType -> IO ()
sendPaymentToPAT bwHandles account requestID address amountType = do
    let dbConn = bhDBConnCH bwHandles
        dbLock = bhDBWriteLock bwHandles
        patHandleMVar = bhPendingActionsTrackerHandleMVar bwHandles
    patHandle <- readMVar patHandleMVar
    expiration <- addUTCTime sendPaymentInterval <$> getCurrentTime
    let action = SendPaymentAction account requestID
                            (adjustAddr address) amountType expiration
    withSerialTransaction dbLock dbConn $ putPendingActions dbConn [action]
    nudgePendingActionsTracker patHandle

sendClientStatus :: BridgewalkerHandles-> AccountCache-> ExchangeStatus -> ClientData-> IO ()
sendClientStatus bwHandles accountCache exchangeStatus client = do
    let dbConn = bhDBConnCH bwHandles
        account = cdAccount client
        answerChan = cdAnswerChan client
    (btcIn, usdBalance, primaryBTCAddress) <-
        getClientDBStatus dbConn (bAccount account)
    let pendingTxs = fromMaybe [] $ M.lookup account accountCache
    let status = ClientStatus { csUSDBalance = usdBalance
                              , csBTCIn = btcIn
                              , csPrimaryBTCAddress = primaryBTCAddress
                              , csPendingTxs = pendingTxs
                              , csExchangeAvailable =
                                    case exchangeStatus of
                                        ExchangeAvailable _ -> True
                                        ExchangeUnavailable -> False
                              , csExchangeRate =
                                    case exchangeStatus of
                                        ExchangeAvailable rate -> rate
                                        ExchangeUnavailable -> 0
                              }
    writeChan answerChan $ ForwardStatusToClient status

sendQuote :: BridgewalkerHandles-> ClientData-> BridgewalkerAccount-> Integer-> AmountType-> IO ()
sendQuote bwHandles client account requestID amountType = do
    let answerChan = cdAnswerChan client
    qc <- compileQuote bwHandles account amountType
    let replyData = case qc of
                        SuccessfulQuote quote -> Just quote
                        _ -> Nothing
    writeChan answerChan $ ForwardQuoteToClient requestID replyData

sendPong :: ClientData -> IO ()
sendPong client = writeChan (cdAnswerChan client) SendPongToClient

sendSuccessfulSend :: ClientData -> Integer -> IO ()
sendSuccessfulSend client requestID =
    writeChan (cdAnswerChan client) $ ForwardSuccessfulSend requestID

sendFailedSend :: ClientData -> Integer -> T.Text -> IO ()
sendFailedSend client requestID reason =
    writeChan (cdAnswerChan client) $ ForwardFailedSend requestID reason

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
        Just v -> return ((v, pendingTx) : augPendingTxs,
                              M.insert addr v updatedAddressCache)
        Nothing -> do
            mAccount <- getAccountByAddress dbConn (adjustAddr addr)
            return ((mAccount, pendingTx) : augPendingTxs,
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
