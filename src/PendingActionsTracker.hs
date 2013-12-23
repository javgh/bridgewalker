{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module PendingActionsTracker
    ( initialPendingActionsState
    , initPendingActionsTracker
    , nudgePendingActionsTracker
    , addPendingActions
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Database.PostgreSQL.Simple
import Data.Time.Clock
import Network.Metricsd.Client
import Network.MtGoxAPI

import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC
import qualified Data.Sequence as S

import AddressUtils
import CommonTypes
import Config
import DbUtils
import FormatUtils
import PendingActionsTrackerQueueManagement
import QuoteUtils
import Utils

import qualified ClientHub as CH

pauseInterval :: NominalDiffTime
pauseInterval = 60  -- pauses are 60 seconds long

mtgoxCommunicationErrorMsg :: String
mtgoxCommunicationErrorMsg = "Currently unable to communicate with Mt.Gox."

mtgoxCommunicationErrorBundle :: Maybe LogContent -> ErrorBundle
mtgoxCommunicationErrorBundle =
    ErrorBundle "Currently unable to communicate with Mt.Gox."

mtgoxRebalancingErrorBundle :: Maybe LogContent -> ErrorBundle
mtgoxRebalancingErrorBundle =
    ErrorBundle "Pausing until rebalancing of reserves is completed."

fatalSendPaymentError :: String
fatalSendPaymentError = "An untimely error has left your account in an\
                        \ inconsistent state. Please contact support.\
                        \ Sorry for the inconvenience!"

data PendingActionsStateModification = RemoveAction
                                     | ReplaceAction BridgewalkerAction
                                        -- this is the replacement action
                                     | ReplaceWithTwoActions
                                            BridgewalkerAction
                                            BridgewalkerAction
                                            -- replacement actions will
                                            -- be executed in the order given
                                     | KeepAction
                                     | AddPauseAction String
                                        -- parameter describes reason for pause

data WithdrawalType = WithdrawBTC { _wtAmount :: Integer }
                    | WithdrawUSD { _wtAmount :: Integer }
                    deriving (Show)

data WithdrawalAction = WithdrawalAction { _waAddress :: RPC.BitcoinAddress
                                         , _waType :: WithdrawalType
                                         }
                        deriving (Show)

data SendPaymentAnswer = SendPaymentSuccessful
                            { _spaAccount :: BridgewalkerAccount
                            , _spaRequestID :: Integer
                            , _spaSerializedTransactionM
                                :: Maybe RPC.SerializedTransaction
                            }
                       | SendPaymentFailed
                            { _spaAccount :: BridgewalkerAccount
                            , _spaRequestID :: Integer
                            , _spaReason :: T.Text
                            }

data ErrorBundle = ErrorBundle { _ebUserMsg :: String
                               , _ebLogMsg :: Maybe LogContent
                               }

data ProcessingState = ProcessingState
                            { _psPAS :: PendingActionsState
                            , _psKeepGoing :: Bool
                            , _psTouchedAccounts :: [BridgewalkerAccount]
                            , _psSendPaymentAnswerM :: Maybe SendPaymentAnswer
                            }
makeLenses ''ProcessingState

initialPendingActionsState :: PendingActionsState
initialPendingActionsState = PendingActionsState { pasSequence = S.empty
                                                 , pasStatus = ""
                                                 , pasBTCImbalance = 0
                                                 , pasLastOrder = Nothing
                                                 }

initPendingActionsTracker :: BridgewalkerHandles -> IO PendingActionsTrackerHandle
initPendingActionsTracker bwHandles = do
    chan <- newChan
    _ <- linkedForkIO $ trackerLoop bwHandles chan
    let handle = PendingActionsTrackerHandle chan
    nudgePendingActionsTracker handle
    return handle

trackerLoop :: BridgewalkerHandles -> Chan () -> IO ()
trackerLoop bwHandles chan =
    let dbConn = bhDBConnPAT bwHandles
        dbLock = bhDBWriteLock bwHandles
        chHandle = bhClientHubHandle bwHandles
        mcHandle = bhMetricsdClient bwHandles
    in forever $ do
        _ <- readChan chan
        startTimestamp <- getCurrentTime
        (touchedAccounts, sendPaymentAnswerM)
            <- withSerialTransaction dbLock dbConn $ do
                paState <- readPendingActionsStateFromDB dbConn
                (paState', keepGoing, touchedAccounts, sendPaymentAnswerM)
                    <- maybeProcessOneAction bwHandles paState
                writePendingActionsStateToDB dbConn paState'
                when keepGoing $ writeChan chan ()
                return (touchedAccounts, sendPaymentAnswerM)
        case sendPaymentAnswerM of
            Nothing -> return ()
            Just (SendPaymentSuccessful account requestID mTx) ->
                CH.signalSuccessfulSend chHandle account requestID mTx
            Just (SendPaymentFailed account requestID reason) ->
                CH.signalFailedSend chHandle account requestID reason
        CH.signalAccountUpdates chHandle touchedAccounts
        stopTimestamp <- getCurrentTime
        let duration = round $ 1000 * diffUTCTime stopTimestamp startTimestamp
                        -- in ms
        sendHistogram mcHandle "bridgewalker_action_time" duration

maybeProcessOneAction :: BridgewalkerHandles-> PendingActionsState-> IO(PendingActionsState,Bool,[BridgewalkerAccount],Maybe SendPaymentAnswer)
maybeProcessOneAction bwHandles paState = do
    let initialState = ProcessingState { _psPAS = paState
                                       , _psKeepGoing = False
                                       , _psTouchedAccounts = []
                                       , _psSendPaymentAnswerM = Nothing
                                       }
    finalState <- execStateT go initialState
    return (finalState ^. psPAS, finalState ^. psKeepGoing,
                finalState ^. psTouchedAccounts,
                finalState ^. psSendPaymentAnswerM)
  where
    go = case popPendingAction paState of
            Nothing -> return ()
            Just (action, paState') -> do
                psPAS .= paState'
                processOneAction bwHandles action

processOneAction :: BridgewalkerHandles-> BridgewalkerAction-> StateT ProcessingState IO ()
processOneAction bwHandles action = do
    modification <- case action of
        DepositAction amount address ->
            processDeposit bwHandles amount address
        ConvertBTCAcount amount account ->
            convertBTC bwHandles amount account
        MarketAction -> balanceBooks bwHandles
        SendPaymentAction account requestID address amountType expiration ->
            sendPayment bwHandles account
                  requestID address amountType expiration
        PauseAction expiration ->
            checkPause expiration
        HeartbeatAction -> signalHeartbeat bwHandles

    case modification of
        RemoveAction -> do
            -- nothing to be done, action has already been popped off;
            -- clear status and keep processing
            clearStatus
            psKeepGoing .= True
        ReplaceAction newAction -> do
            -- add another action in place of the one that was just
            -- removed and keep processing
            psPAS %= \paState -> putPendingAction paState newAction
            clearStatus
            psKeepGoing .= True
        ReplaceWithTwoActions firstNewAction secondNewAction -> do
            -- add two other actions in place of the one that was just
            -- removed and keep processing; make sure to push the second
            -- one first, so that it will be executed after the first one
            psPAS %= \paState -> putPendingAction paState secondNewAction
            psPAS %= \paState -> putPendingAction paState firstNewAction
            clearStatus
            psKeepGoing .= True
        KeepAction ->
            -- put action back in the queue
            psPAS %= \paState -> putPendingAction paState action
        AddPauseAction status -> do
            -- put action back in the queue and also add
            -- a pause action
            psPAS %= \paState -> putPendingAction paState action
            expiration <- addUTCTime pauseInterval <$> liftIO getCurrentTime
            psPAS %= \paState -> putPendingAction paState $ PauseAction expiration
            setStatus status
    return ()
  where
    clearStatus = psPAS %= \paState -> paState { pasStatus = "" }
    setStatus status = psPAS %= \paState -> paState { pasStatus = status }

addTouchedAccount :: BridgewalkerAccount -> StateT ProcessingState IO ()
addTouchedAccount account = psTouchedAccounts %= \accounts -> account : accounts

adjustBTCImbalance :: Integer -> StateT ProcessingState IO ()
adjustBTCImbalance adjustment =
    psPAS %= \paState ->
                let imbalance = pasBTCImbalance paState
                in paState { pasBTCImbalance = imbalance + adjustment }

checkPause :: UTCTime -> StateT ProcessingState IO PendingActionsStateModification
checkPause expiration = do
    now <- liftIO getCurrentTime
    return $ if now >= expiration
                then RemoveAction
                else KeepAction

signalHeartbeat :: BridgewalkerHandles-> StateT ProcessingState IO PendingActionsStateModification
signalHeartbeat bwHandles = do
    let mcHandle = bhMetricsdClient bwHandles
    liftIO $ sendMeter mcHandle "bridgewalker_heartbeat"
    return RemoveAction

processDeposit :: BridgewalkerHandles-> Integer -> RPC.BitcoinAddress -> StateT ProcessingState IO PendingActionsStateModification
processDeposit bwHandles amount address = do
    let dbConn = bhDBConnPAT bwHandles
        logger = bhAppLogger bwHandles
    accountM <- liftIO $ getAccountByAddress dbConn (adjustAddr address)
    case accountM of
        Nothing -> do
            let logMsg = SystemDepositProcessed
                            { lcInfo = "Deposit to system -\
                                       \ no matching account found." }
            liftIO $ logger logMsg
            return RemoveAction
        Just (BridgewalkerAccount account) -> do
            newBalance <- liftIO $ adjustBTCInBalance dbConn account amount
            let logMsg = DepositProcessed
                            { lcAccount = account
                            , lcInfo = formatBTCAmount amount
                                        ++ " BTC deposited into account "
                                        ++ show account
                                        ++ " - balance is now "
                                        ++ formatBTCAmount newBalance ++ " BTC."
                            }
            liftIO $ logger logMsg
            let bwAccount = BridgewalkerAccount account
                action = ConvertBTCAcount
                            { baAmount = newBalance
                            , baAccount = bwAccount
                            }
            addTouchedAccount bwAccount
            return $ ReplaceAction action

convertBTC :: BridgewalkerHandles -> Integer -> BridgewalkerAccount -> StateT ProcessingState IO PendingActionsStateModification
convertBTC bwHandles amount bwAccount = do
    let logger = bhAppLogger bwHandles
        maximumOrderBTC = bcMaximumOrderBTC . bhConfig $ bwHandles
        account = bAccount bwAccount
        adjustedAmount = min maximumOrderBTC amount
        remainingAmount = amount - adjustedAmount
    usdAmountM <- liftIO . runEitherT $ do
                    checkMtGoxIdle bwHandles
                    checkMtGoxWalletNeededBTC bwHandles adjustedAmount
                    compileSimpleQuoteBTCSellWithLog bwHandles adjustedAmount
    case usdAmountM of
        Left (ErrorBundle userMsg logMsgM) -> do    -- either Mt.Gox error
                                                    -- or Mt.Gox low balance
                                                    -- or depth store problems
                                                    -- or Mt.Gox not idle
            whenJust logMsgM $ \logMsg -> liftIO (logger logMsg)
            return $ AddPauseAction userMsg
        Right usdAmount -> do  -- everything looks good,
                               -- we can credit the account
            (newBTCBalance, newUSDBalance) <-
                liftIO $ creditUSDToAccount bwHandles bwAccount
                                            adjustedAmount usdAmount
            addTouchedAccount bwAccount
            adjustBTCImbalance adjustedAmount
            imbalance <- pasBTCImbalance <$> use psPAS
            let info = formatBTCAmount adjustedAmount ++ " BTC credited as "
                        ++ formatUSDAmount usdAmount ++ " USD to account "
                        ++ show account ++ " - balance is now "
                        ++ formatUSDAmount newUSDBalance ++ " USD and "
                        ++ formatBTCAmount newBTCBalance ++ " BTC."
                        ++ " Exchange imbalance is now "
                        ++ formatBTCAmount imbalance ++ " BTC."
            let logMsg = BTCConvertedToFiat { lcAccount = account
                                            , lcInfo = info
                                            , lcImbalance = imbalance
                                            }
            liftIO $ logger logMsg
            if remainingAmount == 0
                then return $ ReplaceAction MarketAction
                else do
                    let secondAction = ConvertBTCAcount
                                        { baAmount = remainingAmount
                                        , baAccount = bwAccount
                                        }
                        info' = "Large deposit of "
                                   ++ formatBTCAmount amount ++ " BTC"
                                   ++ " to account " ++ show account
                                   ++ " had to be split up."
                        logMsg' = LargeDeposit { lcAccount = account
                                               , lcInfo = info'
                                               }
                    liftIO $ logger logMsg'
                    return $ ReplaceWithTwoActions MarketAction secondAction

compileSimpleQuoteBTCSellWithLog :: BridgewalkerHandles -> Integer -> EitherT ErrorBundle IO Integer
compileSimpleQuoteBTCSellWithLog bwHandles adjustedAmount = do
    simpleQuote <- liftIO $ compileSimpleQuoteBTCSell bwHandles adjustedAmount
    case simpleQuote of
        SuccessfulQuote answer -> return answer
        QuoteCompilationError DepthStoreWasUnavailable ->
            left $ mtgoxCommunicationErrorBundle logMsgUnavailable
        QuoteCompilationError HadNotEnoughDepth ->
            left $ mtgoxCommunicationErrorBundle logMsgInsufficient
  where
    logMsgUnavailable = Just $ MtGoxOrderBookUnavailable
                                    "Depth store is unavailable."
    logMsgInsufficient =
        Just $ MtGoxOrderBookInsufficient
                    ("Depth store does not have enough depth to sell "
                    ++ formatBTCAmount adjustedAmount ++ " BTC.")

creditUSDToAccount :: BridgewalkerHandles-> BridgewalkerAccount -> Integer -> Integer -> IO (Integer, Integer)
creditUSDToAccount bwHandles bwAccount totalCost usdAmount = do
    let dbConn = bhDBConnPAT bwHandles
        account = bAccount bwAccount
    btcBalance <- getBTCInBalance dbConn account
    usdBalance <- getUSDBalance dbConn account
    let newBTCBalance = max 0 (btcBalance - totalCost)
        newUSDBalance = usdBalance + usdAmount
    _ <- execute dbConn "update accounts set btc_in=?, usd_balance=?\
                        \ where account_nr=?"
                        (newBTCBalance, newUSDBalance, account)
    return (newBTCBalance, newUSDBalance)

balanceBooks :: BridgewalkerHandles -> StateT ProcessingState IO PendingActionsStateModification
balanceBooks bwHandles = do
    keepGoing <- checkUpOnPreviousOrder bwHandles
    if keepGoing then placeNewOrder bwHandles >> return RemoveAction
                 else return RemoveAction -- retry again with
                                          -- the next MarketAction

-- | Check whether a previous order was submitted, figure out whether
-- it was probably successful and then update the books. Returns whether
-- it is safe to continue with new orders.
checkUpOnPreviousOrder :: BridgewalkerHandles -> StateT ProcessingState IO Bool
checkUpOnPreviousOrder bwHandles = do
    let logger = bhAppLogger bwHandles
    lastOrderM <- pasLastOrder <$> use psPAS
    case lastOrderM of
        Just (PendingOrder opCountPrev adjustment) -> do
            opCountM <- liftIO . runEitherT $ do
                            letMtGoxOrdersExecute bwHandles
                            getOperationsCount bwHandles
            case opCountM of
                Left (ErrorBundle _ logMsgM) -> do
                    whenJust logMsgM $ \logMsg -> liftIO (logger logMsg)
                    -- checking up on the previous order failed
                    -- for some reason; we cannot be sure what happened,
                    -- so it is probably best to wait and try again
                    return False
                Right opCount -> do
                    when (opCount > opCountPrev) $ do
                        -- if the operation count is different, assume that
                        -- the order was successful
                        psPAS %= \paState ->
                                    let imbalancePrev = pasBTCImbalance paState
                                    in paState { pasBTCImbalance =
                                                    imbalancePrev + adjustment }
                        newImbalance <- pasBTCImbalance <$> use psPAS
                        let info = "Previous one shot order was probably\
                                   \ successful. Applying adjustment of "
                                   ++ formatBTCAmount adjustment ++ " BTC."
                                   ++ " Exchange imbalance is now "
                                   ++ formatBTCAmount newImbalance ++ " BTC."
                            logMsg = OneShotOrderProbablySuccessful
                                        { lcInfo = info
                                        , lcAdjustment = adjustment
                                        , lcImbalance = newImbalance
                                        }
                        liftIO $ logger logMsg
                    -- either way, clear the previous order
                    psPAS %= \paState -> paState { pasLastOrder = Nothing }
                    return True
        Nothing -> return True

placeNewOrder :: BridgewalkerHandles -> StateT ProcessingState IO ()
placeNewOrder bwHandles = do
    let minimumOrderBTC = bcMtGoxMinimumOrderBTC . bhConfig $ bwHandles
        maximumOrderBTC = bcMaximumOrderBTC . bhConfig $ bwHandles
        logger = bhAppLogger bwHandles
    imbalance <- pasBTCImbalance <$> use psPAS
    -- check if we need to sell BTC
    when (imbalance >= minimumOrderBTC) $ do
        let amountToSell = min imbalance maximumOrderBTC
        opCountM <- liftIO . runEitherT $ do
                        opCount <- getOperationsCount bwHandles
                        _ <- submitOneShotOrder bwHandles OrderTypeSellBTC
                                                                    amountToSell
                        return opCount
        case opCountM of
            Left (ErrorBundle _ logMsgM) ->
                whenJust logMsgM $ \logMsg -> liftIO (logger logMsg)
                -- if anything should go wrong, we just let
                -- the next MarketAction handle it
            Right opCount -> do
                let lastOrder = PendingOrder opCount (amountToSell * (-1))
                let logMsg = OneShotSellOrderPlaced
                                { lcInfo = "Exchange imbalance is "
                                            ++ formatBTCAmount imbalance
                                            ++ " BTC. Placed order to sell "
                                            ++ formatBTCAmount amountToSell
                                            ++ " BTC at market price. Operation"
                                            ++ " count was " ++ show opCount
                                            ++ " before."
                                , lcOpCount = opCount
                                , lcAmount = amountToSell
                                , lcImbalance = imbalance
                                }
                liftIO $ logger logMsg
                psPAS %= \paState ->paState { pasLastOrder = Just lastOrder }
    -- check if we need to buy BTC
    when (imbalance <= (-1) * minimumOrderBTC) $ do
        let amountToBuy = min (imbalance * (-1)) maximumOrderBTC
        opCountM <- liftIO . runEitherT $ do
                        opCount <- getOperationsCount bwHandles
                        _ <- submitOneShotOrder bwHandles OrderTypeBuyBTC
                                                                    amountToBuy
                        return opCount
        case opCountM of
            Left (ErrorBundle _ logMsgM) ->
                whenJust logMsgM $ \logMsg -> liftIO (logger logMsg)
                -- if anything should go wrong, we just let
                -- the next MarketAction handle it
            Right opCount -> do
                let lastOrder = PendingOrder opCount amountToBuy
                let logMsg = OneShotBuyOrderPlaced
                                { lcInfo = "Exchange imbalance is "
                                            ++ formatBTCAmount imbalance
                                            ++ " BTC. Placed order to buy "
                                            ++ formatBTCAmount amountToBuy
                                            ++ " BTC at market price. Operation"
                                            ++ " count was " ++ show opCount
                                            ++ " before."
                                , lcOpCount = opCount
                                , lcAmount = amountToBuy
                                , lcImbalance = imbalance
                                }
                liftIO $ logger logMsg
                psPAS %= \paState ->paState { pasLastOrder = Just lastOrder }
    return ()

submitOneShotOrder :: BridgewalkerHandles-> OrderType -> Integer -> EitherT ErrorBundle IO Order
submitOneShotOrder bwHandles orderType amount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        (f, method, action) =
            case orderType of
                OrderTypeBuyBTC -> ( submitBtcBuyOrder mtgoxHandles amount
                                   , "submitBtcBuyOrder"
                                   , "submitting a one shot order"
                                   )
                OrderTypeSellBTC -> ( submitBtcSellOrder mtgoxHandles amount
                                    , "submitBtcSellOrder"
                                    , "submitting a one shot order"
                                    )
    liftIO f >>= bundleMtGoxError method action

getOperationsCount :: BridgewalkerHandles -> EitherT ErrorBundle IO Integer
getOperationsCount bwHandles = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
    privateInfo <- liftIO (getPrivateInfoR mtgoxHandles)
                        >>= bundleMtGoxError "getPrivateInfoR"
                                             "getting operations count"
    return $ piUsdOperations privateInfo

letMtGoxOrdersExecute :: BridgewalkerHandles -> EitherT ErrorBundle IO ()
letMtGoxOrdersExecute bwHandles = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
    remainingOrdersM <- liftIO $ letOrdersExecuteBrieflyR mtgoxHandles
    case remainingOrdersM of
        Left errMsg ->
            let logMsg = MtGoxError $ "While doing letMtGoxOrdersExecute: "
                                        ++ errMsg
            in left $ mtgoxCommunicationErrorBundle (Just logMsg)
        Right count ->
            when (count > 0) $
                let logMsg = MtGoxStillOpenOrders
                              "Even after calling letOrdersExecuteBrieflyR\
                              \ there are still open orders."
                in left $ mtgoxCommunicationErrorBundle (Just logMsg)

letOrdersExecuteBrieflyR :: MtGoxAPIHandles -> IO (Either String Integer)
letOrdersExecuteBrieflyR mtgoxHandles =
    go (1 :: Integer)   -- makes two attempts (1 and 0);
                        -- should not block longer than 40 seconds
  where
    go 0 = do
        oocM <- getOrderCountR mtgoxHandles
        case oocM of
            Left errMsg -> return $ Left errMsg
            Right (OpenOrderCount count) -> return $ Right count
    go countdown = do
        oocM <- getOrderCountR mtgoxHandles
        case oocM of
            Left _ -> go (countdown - 1)
            Right (OpenOrderCount count) ->
                if count > 0 then go (countdown - 1)
                             else return $ Right 0

sendPayment :: BridgewalkerHandles-> BridgewalkerAccount-> Integer-> RPC.BitcoinAddress-> AmountType-> UTCTime-> StateT ProcessingState IO PendingActionsStateModification
sendPayment bwHandles account requestID address amountType expiration = do
    let logger = bhAppLogger bwHandles
    result <- liftIO $ runEitherT go
    case result of
        Left errMsg -> do
            let answer = SendPaymentFailed account requestID (T.pack errMsg)
                logMsg = SendPaymentFailedCheck
                            { lcAccount = bAccount account
                            , lcAddress = T.unpack (adjustAddr address)
                            , lcInfo = errMsg
                            }
            liftIO $ logger logMsg
            psSendPaymentAnswerM .= Just answer
            return RemoveAction
        Right (touchedAccounts, adjustment, mTx) -> do
            let answer = SendPaymentSuccessful account requestID mTx
            psSendPaymentAnswerM .= Just answer
            mapM_ addTouchedAccount touchedAccounts
            if adjustment == 0
                then return RemoveAction
                else do
                    adjustBTCImbalance adjustment
                    imbalance <- pasBTCImbalance <$> use psPAS
                    let info = "After sending out Bitcoin payment,\
                               \ the exchange imbalance is now "
                               ++ formatBTCAmount imbalance ++ " BTC."
                        logMsg = ImbalanceAdjustedAfterSending
                                    { lcInfo = info
                                    , lcImbalance = imbalance
                                    }
                    liftIO $ logger logMsg
                    return $ ReplaceAction MarketAction
  where
    go = do
            let dbConn = bhDBConnPAT bwHandles
            now <- liftIO getCurrentTime
            tryAssert busyMsg (now < expiration)
            quoteData <- sendPaymentPreparationChecks bwHandles account
                                                            address amountType
            tryAssert busyMsg (now < expiration) -- check again, as some
                                                 -- previous checks might
                                                 -- have blocked for a while
            otherAccountM <- liftIO $ getAccountByAddress
                                            dbConn (adjustAddr address)
            case otherAccountM of
                Just otherAccount -> do
                    performInternalTransfer bwHandles account
                                            otherAccount amountType quoteData
                    return ([account, otherAccount], 0, Nothing)
                Nothing -> do
                    sendPaymentExternalPreparationChecks bwHandles quoteData
                    tryAssert busyMsg (now < expiration) -- one final check
                    convertFiat bwHandles account quoteData
                    let btcAmount = qdBTC quoteData
                    mTx <- sendBTC bwHandles account address btcAmount
                    return ([account], (-1) * btcAmount, mTx)
    busyMsg = "The server is very busy at the moment. Please try again later."

performInternalTransfer :: BridgewalkerHandles-> BridgewalkerAccount-> BridgewalkerAccount-> AmountType-> QuoteData-> EitherT String IO ()
performInternalTransfer bwHandles bwAccount bwOtherAccount amountType quoteData = do
    let usdAmount = case amountType of
                        AmountBasedOnUSDAfterFees _ -> qdUSDAccount quoteData
                            -- the value of qdUSDRecipient takes fees into
                            -- account, that do not apply here; therefore use
                            -- the amount given by the user, which is
                            -- qdUSDAccount
                        _ -> qdUSDRecipient quoteData
        dbConn = bhDBConnPAT bwHandles
        logger = bhAppLogger bwHandles
        account = bAccount bwAccount
        otherAccount = bAccount bwOtherAccount
    tryAssert "Sending to self seems unnecessary." (account /= otherAccount)
    usdBalance <- liftIO $ getUSDBalance dbConn account
    otherUSDBalance <- liftIO $ getUSDBalance dbConn otherAccount
    let newUSDBalance = usdBalance - usdAmount
        otherNewUSDBalance = otherUSDBalance + usdAmount
        info = "Transferred " ++ formatUSDAmount usdAmount ++ " USD"
                ++ " directly from account " ++ show account
                ++ " to account " ++ show otherAccount ++ "."
        logMsg = InternalTransfer { lcAccount = account
                                  , lcOtherAccount = otherAccount
                                  , lcAmount = usdAmount
                                  , lcInfo = info
                                  }
    liftIO $ logger logMsg
    liftIO . void $ execute dbConn "update accounts set usd_balance=?\
                                   \ where account_nr=?"
                                   (otherNewUSDBalance, otherAccount)
    liftIO . void $ execute dbConn "update accounts set usd_balance=?\
                                   \ where account_nr=?"
                                   (newUSDBalance, account)
    return ()

sendBTC :: BridgewalkerHandles-> BridgewalkerAccount-> RPC.BitcoinAddress-> Integer-> EitherT String IO (Maybe RPC.SerializedTransaction)
sendBTC bwHandles bwAccount address btcAmountToSend = do
    let rpcAuth = bcRPCAuth . bhConfig $ bwHandles
        account = bAccount bwAccount
        logger = bhAppLogger bwHandles
        watchdogLogger = bhWatchdogLogger bwHandles
    rpcResult <- liftIO $ RPC.sendToAddress rpcAuth
                                    address (adjustAmount btcAmountToSend)
    case rpcResult of
        Left networkOrParseError -> do
            let logMsg = BTCSendNetworkOrParseError
                            { lcAccount = account
                            , lcAddress = T.unpack . RPC.btcAddress $ address
                            , lcAmount = btcAmountToSend
                            , lcInfo = networkOrParseError
                            }
            liftIO $ logger logMsg
            left fatalSendPaymentError
        Right (Left sendError) -> do
            let logMsg = BTCSendError
                            { lcAccount = account
                            , lcAddress = T.unpack . RPC.btcAddress $ address
                            , lcAmount = btcAmountToSend
                            , lcInfo = show sendError
                            }
            liftIO $ logger logMsg
            left fatalSendPaymentError
        Right (Right txID) -> do
            let info = "Account " ++ show account ++ " sent out "
                        ++ formatBTCAmount btcAmountToSend ++ " BTC to "
                        ++ (T.unpack . RPC.btcAddress $ address)
                        ++ " with transaction "
                        ++ (T.unpack . RPC.btcTxID $ txID) ++ "."
                logMsg = BTCSent { lcAccount = account
                                 , lcInfo = info
                                  }
            liftIO $ logger logMsg
            mTx <- liftIO $ RPC.getRawTransactionR (Just watchdogLogger) rpcAuth
                                                    txID
            return mTx

convertFiat :: BridgewalkerHandles-> BridgewalkerAccount -> QuoteData -> EitherT String IO ()
convertFiat bwHandles bwAccount quoteData = do
    let logger = bhAppLogger bwHandles
        account = bAccount bwAccount
        btcAmount = qdBTC quoteData
        usdAmount = qdUSDAccount quoteData
    -- debit the account
    newUSDBalance <- debitUSDFromAccount bwHandles bwAccount usdAmount
    let info = formatUSDAmount usdAmount ++ " USD debited from account "
                ++ show account ++ " to send out "
                ++ formatBTCAmount btcAmount ++ " BTC - balance is now "
                ++ formatUSDAmount newUSDBalance ++ " USD."
    let logMsg = FiatConvertedToBTC { lcAccount = account
                                    , lcInfo = info
                                    }
    liftIO $ logger logMsg
    return ()

debitUSDFromAccount :: BridgewalkerHandles -> BridgewalkerAccount -> Integer -> EitherT String IO Integer
debitUSDFromAccount bwHandles bwAccount usdAmount = do
    let dbConn = bhDBConnPAT bwHandles
        account = bAccount bwAccount
    usdBalance <- liftIO $ getUSDBalance dbConn account
    let newUSDBalance = usdBalance - usdAmount
    liftIO . void $ execute dbConn "update accounts set usd_balance=?\
                                   \ where account_nr=?"
                                   (newUSDBalance, account)
    return newUSDBalance

sendPaymentPreparationChecks :: BridgewalkerHandles-> BridgewalkerAccount-> RPC.BitcoinAddress-> AmountType-> EitherT String IO QuoteData
sendPaymentPreparationChecks bwHandles account address amountType = do
    _ <- checkAddress bwHandles address
    qc <- liftIO $ compileQuote bwHandles account amountType
    quoteData <- case qc of
                    SuccessfulQuote qd -> return qd
                    QuoteCompilationError HadNotEnoughDepth ->
                        left "The entered amount is too large."
                    QuoteCompilationError DepthStoreWasUnavailable ->
                        left mtgoxCommunicationErrorMsg
    tryAssert "Insufficient funds to complete the transaction." $
                    qdSufficientBalance quoteData
    return quoteData

sendPaymentExternalPreparationChecks :: BridgewalkerHandles -> QuoteData -> EitherT String IO ()
sendPaymentExternalPreparationChecks bwHandles quoteData = do
    let logger = bhAppLogger bwHandles
    checkOrderRange quoteData bwHandles
    liftIO . logger $ LogMisc "About to call checkMtGoxWalletNeededUSD'"
    checkMtGoxWalletNeededUSD' bwHandles (qdUSDAccount quoteData)
    liftIO . logger $ LogMisc "After call to checkMtGoxWalletNeededUSD'"
    liftIO . logger $ LogMisc "About to call checkBitcoindWallet"
    checkBitcoindWallet bwHandles (qdBTC quoteData)
    liftIO . logger $ LogMisc "After call to checkBitcoindWallet'"
    return ()
  where
    checkMtGoxWalletNeededUSD' bwHandles' amount =
        fmapLT (\(ErrorBundle userMsg _) -> userMsg) $
                    checkMtGoxWalletNeededUSD bwHandles' amount

checkOrderRange :: QuoteData -> BridgewalkerHandles -> EitherT String IO ()
checkOrderRange quoteData bwHandles = do
    let maximumOrderBTC = bcMaximumOrderBTC . bhConfig $ bwHandles
        maximumOrderBTCStr = formatBTCAmount maximumOrderBTC ++ " BTC"
        minimumOrderBTC = 1 :: Integer
        minimumOrderBTCStr = formatBTCAmount minimumOrderBTC ++ " BTC"
        btcAmount = qdBTC quoteData
    tryAssert ("Currently the maximum transaction size is "
                ++ maximumOrderBTCStr ++ ".") $ btcAmount <= maximumOrderBTC
    tryAssert ("The minimum transaction size is "
                ++ minimumOrderBTCStr ++ ".") $ btcAmount >= minimumOrderBTC

checkMtGoxWalletNeededBTC :: BridgewalkerHandles -> Integer -> EitherT ErrorBundle IO ()
checkMtGoxWalletNeededBTC bwHandles amount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        safetyMarginBTC = bcSafetyMarginBTC . bhConfig $ bwHandles
    privateInfo <- liftIO (getPrivateInfoR mtgoxHandles)
                        >>= bundleMtGoxError "getPrivateInfoR"
                                             "checking BTC balance"
    tryAssert (mtgoxRebalancingErrorBundle logMsgLowBalance)
                (piBtcBalance privateInfo >= safetyMarginBTC + amount)
    return ()
  where
    logMsgLowBalance = Just $ MtGoxLowBTCBalance "Mt.Gox BTC balance is low."

checkMtGoxWalletNeededUSD :: BridgewalkerHandles -> Integer -> EitherT ErrorBundle IO ()
checkMtGoxWalletNeededUSD bwHandles amount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        safetyMarginUSD = bcSafetyMarginUSD . bhConfig $ bwHandles
    privateInfo <- liftIO (getPrivateInfoR mtgoxHandles)
                        >>= bundleMtGoxError "getPrivateInfoR"
                                             "checking USD balance"
    tryAssert (ErrorBundle "The server's hot wallet is running low."
                            logMsgLowBalance)
                (piUsdBalance privateInfo >= safetyMarginUSD + amount)
    return ()
  where
    logMsgLowBalance = Just $ MtGoxLowUSDBalance
                                    "Mt.Gox hot wallet is running low."

checkMtGoxIdle :: BridgewalkerHandles -> EitherT ErrorBundle IO ()
checkMtGoxIdle bwHandles = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
    OpenOrderCount count <- liftIO (getOrderCountR mtgoxHandles)
                                >>= bundleMtGoxError
                                        "getOrderCountR"
                                        "checking whether Mt.Gox is idle"
    tryAssert (ErrorBundle "The server is waiting for\
                           \ scheduled trades to execute."
                           logMsgOpenOrders)
              (count == 0)
    return ()
  where
    logMsgOpenOrders =
        Just $ MtGoxStillOpenOrders "BTC conversion has been paused,\
                                    \ because Mt.Gox is still executing\
                                    \ previous trades."

checkBitcoindWallet :: BridgewalkerHandles -> Integer -> EitherT String IO ()
checkBitcoindWallet bwHandles neededBTCAmount = do
    let rpcAuth = bcRPCAuth . bhConfig $ bwHandles
        safetyMarginBTC = bcSafetyMarginBTC . bhConfig $ bwHandles
        watchdogLogger = bhWatchdogLogger bwHandles
    btcSystemBalance <- liftIO $ RPC.getBalanceR (Just watchdogLogger) rpcAuth
                                                     confsNeededForSending
    tryAssert "The server is currently busy rebalancing its reserves.\
              \ Please try again later."
              (neededBTCAmount + safetyMarginBTC
                    <= adjustAmount btcSystemBalance)
    return ()

checkAddress :: BridgewalkerHandles-> RPC.BitcoinAddress -> EitherT String IO RPC.BitcoinAddress
checkAddress bwHandles address = do
    let watchdogLogger = bhWatchdogLogger bwHandles
        rpcAuth = bcRPCAuth . bhConfig $ bwHandles
        isNotNull = not $ T.null (adjustAddr address)
    tryAssert "No Bitcoin address was given." isNotNull
    info <- liftIO $ RPC.validateAddressR (Just watchdogLogger) rpcAuth address
    tryAssert "This does not seem to be a valid Bitcoin address."
                    (RPC.baiIsValid info)
    return address

-- | Given a method name and a description of what was attempted, will bundle
-- the error up using 'mtgoxCommunicationErrorBundle'.
bundleMtGoxError :: Monad m => String -> String -> Either String a -> EitherT ErrorBundle m a
bundleMtGoxError method action r =
    case r of
        Left errMsg ->
            let logMsg = MtGoxError $ "Unable to call " ++ method ++ " while "
                                      ++ action ++ ". Details: " ++ errMsg
            in left $ mtgoxCommunicationErrorBundle (Just logMsg)
        Right v -> return v
