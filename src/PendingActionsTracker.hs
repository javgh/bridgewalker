{-# LANGUAGE OverloadedStrings #-}
module PendingActionsTracker
    ( initialPendingActionsState
    , initPendingActionsTracker
    , nudgePendingActionsTracker
    , addPendingActions
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Data.Time.Clock
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

import qualified ClientHub as CH

pauseInterval :: NominalDiffTime
pauseInterval = 60  -- pauses are 60 seconds long

mtgoxCommunicationErrorMsg :: String
mtgoxCommunicationErrorMsg = "Currently unable to communicate with Mt.Gox."

fatalSendPaymentError :: String
fatalSendPaymentError = "An untimely error has left your account in an\
                        \ inconsistent state. Please contact support.\
                        \ Sorry for the inconvenience!"

data PendingActionsStateModification = RemoveAction
                                     | ReplaceAction BridgewalkerAction
                                        -- this is the replacement action
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

data SellOrderProblem = MtGoxLowBalance | MtGoxCallError String

data SendPaymentAnswer = SendPaymentSuccessful
                            { _spaAccount :: BridgewalkerAccount
                            , _spaRequestID :: Integer
                            }
                       | SendPaymentFailed
                            { _spaAccount :: BridgewalkerAccount
                            , _spaRequestID :: Integer
                            , _spaReason :: T.Text
                            }

initialPendingActionsState :: PendingActionsState
initialPendingActionsState = PendingActionsState { pasSequence = S.empty
                                                 , pasStatus = ""
                                                 }

initPendingActionsTracker :: BridgewalkerHandles -> IO PendingActionsTrackerHandle
initPendingActionsTracker bwHandles = do
    chan <- newChan
    _ <- forkIO $ trackerLoop bwHandles chan
    let handle = PendingActionsTrackerHandle chan
    nudgePendingActionsTracker handle
    return handle

trackerLoop :: BridgewalkerHandles -> Chan () -> IO ()
trackerLoop bwHandles chan =
    let dbConn = bhDBConnPAT bwHandles
        dbLock = bhDBWriteLock bwHandles
        chHandle = bhClientHubHandle bwHandles
    in forever $ do
        _ <- readChan chan
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
            Just (SendPaymentSuccessful account requestID) ->
                CH.signalSuccessfulSend chHandle account requestID
            Just (SendPaymentFailed account requestID reason) ->
                CH.signalFailedSend chHandle account requestID reason
        CH.signalAccountUpdates chHandle touchedAccounts

maybeProcessOneAction :: BridgewalkerHandles-> PendingActionsState-> IO(PendingActionsState,Bool,[BridgewalkerAccount],Maybe SendPaymentAnswer)
maybeProcessOneAction bwHandles paState =
     case popPendingAction paState of
            Nothing -> return (paState, False, [], Nothing)
            Just (action, paState') -> processOneAction bwHandles action paState'

processOneAction :: BridgewalkerHandles-> BridgewalkerAction-> PendingActionsState-> IO(PendingActionsState,Bool,[BridgewalkerAccount],Maybe SendPaymentAnswer)
processOneAction bwHandles action paState' = do
    (modification, touchedAccounts, sendPaymentAnswerM) <- case action of
        DepositAction amount address ->
            processDeposit bwHandles amount address
        SellBTCAction amount account ->
            sellBTC bwHandles amount account
        SendPaymentAction account requestID address amountType expiration ->
            sendPayment bwHandles account
                            requestID address amountType expiration
        PauseAction expiration ->
            checkPause expiration
    case modification of
        RemoveAction ->
            -- nothing to be done, action has already been popped off;
            -- clear status and keep processing
            return (paState' { pasStatus = "" }, True
                        , touchedAccounts, sendPaymentAnswerM)
        ReplaceAction newAction ->
            -- add another action in place of the one that was just
            -- removed and keep processing
            let paState'' = putPendingAction paState' newAction
            in return (paState'' { pasStatus = "" }, True
                            , touchedAccounts, sendPaymentAnswerM)
        KeepAction ->
            -- put action back in the queue
            return (putPendingAction paState' action, False
                        , touchedAccounts, sendPaymentAnswerM)
        AddPauseAction status -> do
            -- put action back in the queue and also add
            -- a pause action
            let paState'' = putPendingAction paState' action
            expiration <- addUTCTime pauseInterval <$> getCurrentTime
            let paState''' = putPendingAction paState'' $ PauseAction expiration
            return (paState''' { pasStatus = status }, False
                        , touchedAccounts, sendPaymentAnswerM)

checkPause :: UTCTime -> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
checkPause expiration = do
    now <- getCurrentTime
    return $ if now >= expiration
                then (RemoveAction, [], Nothing)
                else (KeepAction, [], Nothing)

processDeposit :: BridgewalkerHandles-> Integer -> RPC.BitcoinAddress -> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
processDeposit bwHandles amount address = do
    let dbConn = bhDBConnPAT bwHandles
        logger = bhAppLogger bwHandles
    accountM <- getAccountByAddress dbConn (adjustAddr address)
    case accountM of
        Nothing -> do
            let logMsg = SystemDepositProcessed
                            { lcInfo = "Deposit to system -\
                                       \ no matching account found." }
            logger logMsg
            return (RemoveAction, [], Nothing)
        Just (BridgewalkerAccount account) -> do
            btcBalance <- getBTCInBalance dbConn account
            let newBalance = btcBalance + amount
            _ <- execute dbConn
                        "update accounts set btc_in=? where account_nr=?"
                        (newBalance, account)
            let logMsg = DepositProcessed
                            { lcAccount = account
                            , lcInfo = formatBTCAmount amount
                                        ++ " BTC deposited into account "
                                        ++ show account
                                        ++ " - balance is now "
                                        ++ formatBTCAmount newBalance ++ " BTC."
                            }
            logger logMsg
            let bwAccount = BridgewalkerAccount account
                action = SellBTCAction
                            { baAmount = newBalance
                            , baAccount = bwAccount
                            }
            return (ReplaceAction action, [bwAccount], Nothing)

sellBTC :: BridgewalkerHandles-> Integer-> BridgewalkerAccount-> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
sellBTC bwHandles amount bwAccount =
    let minimumOrderBTC = bcMtGoxMinimumOrderBTC . bhConfig $ bwHandles
    in if amount >= minimumOrderBTC
        then sellBTCViaMtGox bwHandles amount bwAccount
        else sellBTCViaSmallTxFund bwHandles amount bwAccount

sellBTCViaSmallTxFund :: BridgewalkerHandles-> Integer-> BridgewalkerAccount-> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
sellBTCViaSmallTxFund bwHandles btcAmount bwAccount = do
    let logger = bhAppLogger bwHandles
    result <- runEitherT $
                tryToSellBTCViaSmallTxFund bwHandles btcAmount bwAccount
    case result of
        Left errMsg -> do
            let logMsg = SmallTxFundWarning
                            { lcInfo = "Problems while trying to sell via\
                                       \small tx fund, trying again later.\
                                       \Error was: " ++ errMsg }
            logger logMsg
            return (AddPauseAction "Communication problems with Mt.Gox\
                                   \ - pausing until it is resolved."
                                   , [], Nothing)
        Right answer -> return answer

tryToSellBTCViaSmallTxFund :: BridgewalkerHandles-> Integer-> BridgewalkerAccount-> EitherT String IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
tryToSellBTCViaSmallTxFund bwHandles btcAmount bwAccount = do
    let logger = bhAppLogger bwHandles
        dbConn = bhDBConnPAT bwHandles
    balanceSmallTxFund bwHandles
    simpleQuote <- liftIO $
                        compileSimpleQuoteBTCSell bwHandles btcAmount
    usdChange <- case simpleQuote of
                    SuccessfulQuote answer -> return answer
                    _ -> left mtgoxCommunicationErrorMsg
    btcTotal <- liftIO $ getSmallTxFundBTCTotal dbConn
    usdTotal <- liftIO $ getSmallTxFundUSDTotal dbConn
    let btcTotal' = btcTotal + btcAmount
        usdTotal' = usdTotal - usdChange
        desc = "Added " ++ formatBTCAmount btcAmount ++ " BTC "
                ++ "and credited user account "
                ++ formatUSDAmount usdChange ++ " USD; new totals are "
                ++ formatBTCAmount btcTotal' ++ " BTC and "
                ++ formatUSDAmount usdTotal' ++ " USD."
        logMsg' = SmallTxFundAction { lcBTCChange = btcAmount
                                    , lcUSDChange = (-1) * usdChange
                                    , lcBTCTotal = btcTotal'
                                    , lcUSDTotal = usdTotal'
                                    , lcInfo = desc
                                    }
    liftIO $ logger logMsg'
    liftIO . void $ execute dbConn "insert into small_tx_fund\
                        \ (timestamp, description\
                        \ , btc_change, usd_change\
                        \ , btc_total, usd_total)\
                        \ values (?, ?, ?, ?, ?, ?)"
                        ("Now" :: String, desc, btcAmount,
                            (-1) * usdChange, btcTotal', usdTotal')
    _ <- liftIO $ creditAccount bwHandles bwAccount btcAmount usdChange True
    return (RemoveAction, [bwAccount], Nothing)

sellBTCViaMtGox :: BridgewalkerHandles-> Integer-> BridgewalkerAccount-> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
sellBTCViaMtGox bwHandles amount bwAccount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        logger = bhAppLogger bwHandles
        safetyMarginBTC = bcSafetyMarginBTC . bhConfig $ bwHandles
        maximalOrderBTC = bcMaximalOrderBTC . bhConfig $ bwHandles
        account = bAccount bwAccount
        adjustedAmount = min maximalOrderBTC amount
        remainingAmount = amount - adjustedAmount
    sell <- tryToExecuteSellOrder mtgoxHandles safetyMarginBTC adjustedAmount
    case sell of
        Left (MtGoxCallError msg) -> do
            let logMsg = MtGoxError
                            { lcInfo = "Error communicating with Mt.Gox while\
                                       \ attempting to sell BTC. Error was: \
                                       \ " ++ msg }
            logger logMsg
            return (AddPauseAction "Communication problems with Mt.Gox\
                                   \ - pausing until it is resolved."
                                   , [], Nothing)
        Left MtGoxLowBalance -> do
            let logMsg = MtGoxLowBTCBalance
                        { lcInfo = "Postponing BTC sell order because of low\
                                   \ balance in Mt.Gox account." }
            logger logMsg
            return (AddPauseAction "Pausing until rebalancing of\
                                   \ reserves is completed.", [], Nothing)
        Right stats -> do
            let usdAmount = max 0 (usdEarned stats - usdFee stats)
            _ <- creditAccount bwHandles bwAccount
                        adjustedAmount usdAmount False
            if remainingAmount == 0
                then return (RemoveAction, [bwAccount], Nothing)
                else do
                    let action = SellBTCAction
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
                    logger logMsg'
                    return (ReplaceAction action, [bwAccount], Nothing)

creditAccount :: BridgewalkerHandles-> BridgewalkerAccount -> Integer -> Integer -> Bool -> IO ()
creditAccount bwHandles bwAccount totalCost usdAmount viaSmallTxFund = do
    let dbConn = bhDBConnPAT bwHandles
        account = bAccount bwAccount
        logger = bhAppLogger bwHandles
    btcBalance <- getBTCInBalance dbConn account
    usdBalance <- getUSDBalance dbConn account
    let newBTCBalance = max 0 (btcBalance - totalCost)
        newUSDBalance = usdBalance + usdAmount
    _ <- execute dbConn "update accounts set btc_in=?, usd_balance=?\
                        \ where account_nr=?"
                        (newBTCBalance, newUSDBalance, account)
    let info = formatBTCAmount totalCost
                    ++ " BTC sold "
                    ++ if viaSmallTxFund
                            then "via small tx fund"
                            else "on Mt.Gox"
                    ++ " and credited "
                    ++ formatUSDAmount usdAmount ++ " USD to account "
                    ++ show account ++ " - balance is now "
                    ++ formatUSDAmount newUSDBalance ++ " USD and "
                    ++ formatBTCAmount newBTCBalance ++ " BTC."
        logMsg = BTCSold
                    { lcAccount = account
                    , lcInfo = info
                    }
    logger logMsg


sendPayment :: BridgewalkerHandles-> BridgewalkerAccount-> Integer-> RPC.BitcoinAddress-> AmountType-> UTCTime-> IO(PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
sendPayment bwHandles account requestID address amountType expiration = do
    let logger = bhAppLogger bwHandles
    result <- runEitherT go
    case result of
        Left errMsg -> do
            let answer = SendPaymentFailed account requestID (T.pack errMsg)
                logMsg = SendPaymentFailedCheck
                            { lcAccount = bAccount account
                            , lcAddress = T.unpack (adjustAddr address)
                            , lcInfo = errMsg
                            }
            logger logMsg
            return (RemoveAction, [], Just answer)
        Right touchedAccounts ->
            let answer = SendPaymentSuccessful account requestID
            in return (RemoveAction, touchedAccounts, Just answer)
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
                    return [account, otherAccount]
                Nothing -> do
                    sendPaymentExternalPreparationChecks bwHandles quoteData
                    tryAssert busyMsg (now < expiration) -- one final check
                    btcAmountToSend <-
                        let buyAction = if qdNeedsSmallTxFund quoteData
                                        then buyBTCViaSmallTxFund
                                        else buyBTC
                        in buyAction bwHandles account quoteData
                    _ <- sendBTC bwHandles account address btcAmountToSend
                    return [account]
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

sendBTC :: BridgewalkerHandles-> BridgewalkerAccount-> RPC.BitcoinAddress-> Integer-> EitherT String IO RPC.TransactionID
sendBTC bwHandles bwAccount address btcAmountToSend = do
    let rpcAuth = bcRPCAuth . bhConfig $ bwHandles
        account = bAccount bwAccount
        logger = bhAppLogger bwHandles
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
            return txID

buyBTCViaSmallTxFund :: BridgewalkerHandles -> BridgewalkerAccount -> QuoteData -> EitherT String IO Integer
buyBTCViaSmallTxFund bwHandles bwAccount quoteData = do
    let logger = bhAppLogger bwHandles
        dbConn = bhDBConnPAT bwHandles
        account = bAccount bwAccount
        typicalTxFee = bcTypicalTxFee . bhConfig $ bwHandles
        btcAmount = qdBTC quoteData
    balanceSmallTxFund bwHandles
    let btcWithFee = btcAmount + typicalTxFee
    simpleQuote <- liftIO $ compileSimpleQuoteBTCBuy bwHandles btcWithFee
    usdChange <- case simpleQuote of
                    SuccessfulQuote answer -> return answer
                    _ -> left mtgoxCommunicationErrorMsg
    let logMsg = BTCBought
                    { lcAccount = account
                    , lcUSDSpent = usdChange
                    , lcUSDFee = 0
                    , lcUSDExtraFee = 0
                    , lcInfo = formatBTCAmount btcAmount ++ " BTC + "
                                ++ formatBTCAmount typicalTxFee ++ " BTC "
                                ++ " bought from small tx fund"
                                ++ " for a total cost of "
                                ++ formatUSDAmount usdChange ++ " USD."
                    }
    liftIO $ logger logMsg
    btcTotal <- liftIO $ getSmallTxFundBTCTotal dbConn
    usdTotal <- liftIO $ getSmallTxFundUSDTotal dbConn
    let btcTotal' = btcTotal - btcWithFee
        usdTotal' = usdTotal + usdChange
        desc = "Used " ++ formatBTCAmount btcWithFee ++ " BTC "
                ++ "(includes typical tx fee of "
                ++ formatBTCAmount typicalTxFee ++ " BTC) "
                ++ "and debited user account "
                ++ formatUSDAmount usdChange ++ " USD; new totals are "
                ++ formatBTCAmount btcTotal' ++ " BTC and "
                ++ formatUSDAmount usdTotal' ++ " USD."
        logMsg' = SmallTxFundAction { lcBTCChange = (-1) * btcWithFee
                                    , lcUSDChange = usdChange
                                    , lcBTCTotal = btcTotal'
                                    , lcUSDTotal = usdTotal'
                                    , lcInfo = desc
                                    }
    liftIO $ logger logMsg'
    liftIO . void $ execute dbConn "insert into small_tx_fund\
                        \ (timestamp, description, btc_change, usd_change\
                        \ , btc_total, usd_total)\
                        \ values (?, ?, ?, ?, ?, ?)"
                        ("Now" :: String, desc, (-1) * btcWithFee,
                            usdChange, btcTotal', usdTotal')
    _ <- debitAccount bwHandles bwAccount usdChange btcWithFee
    return btcAmount

balanceSmallTxFund :: BridgewalkerHandles -> EitherT String IO ()
balanceSmallTxFund bwHandles = do
    let minimumOrderBTC = bcMtGoxMinimumOrderBTC . bhConfig $ bwHandles
        dbConn = bhDBConnPAT bwHandles
    btcTotal <- liftIO $ getSmallTxFundBTCTotal dbConn
    when (btcTotal < minimumOrderBTC) $ increaseSmallTxFund bwHandles
    when (btcTotal > 2 * minimumOrderBTC) $ decreaseSmallTxFund bwHandles

increaseSmallTxFund :: BridgewalkerHandles -> EitherT String IO ()
increaseSmallTxFund bwHandles = do
    let minimumOrderBTC = bcMtGoxMinimumOrderBTC . bhConfig $ bwHandles
        dbConn = bhDBConnPAT bwHandles
        mtgoxHandles = bhMtGoxHandles bwHandles
        logger = bhAppLogger bwHandles
    checkMtGoxWallet bwHandles 0
    btcTotal <- liftIO $ getSmallTxFundBTCTotal dbConn
    usdTotal <- liftIO $ getSmallTxFundUSDTotal dbConn
    mtgoxOrder <- liftIO $ callHTTPApi mtgoxHandles submitOrder
                                            OrderTypeBuyBTC minimumOrderBTC
    orderStats <- case mtgoxOrder of
                    Left orderErr -> do
                        let logMsg = MtGoxError orderErr
                        liftIO $ logger logMsg
                        left mtgoxCommunicationErrorMsg
                    Right stats -> return stats
    let totalCost = usdSpent orderStats + usdFee orderStats
        btcTotal' = btcTotal + minimumOrderBTC
        usdTotal' = usdTotal - totalCost
        desc = "Bought " ++ formatBTCAmount minimumOrderBTC ++ " BTC for "
                    ++ formatUSDAmount totalCost ++ " USD; new totals are "
                    ++ formatBTCAmount btcTotal' ++ " BTC and "
                    ++ formatUSDAmount usdTotal' ++ " USD."
        logMsg = SmallTxFundAction { lcBTCChange = minimumOrderBTC
                                   , lcUSDChange = (-1) * totalCost
                                   , lcBTCTotal = btcTotal'
                                   , lcUSDTotal = usdTotal'
                                   , lcInfo = desc
                                   }
    liftIO $ logger logMsg
    liftIO . void $ execute dbConn "insert into small_tx_fund\
                        \ (timestamp, description, btc_change, usd_change\
                        \ , btc_total, usd_total)\
                        \ values (?, ?, ?, ?, ?, ?)"
                        ("Now" :: String, desc, minimumOrderBTC,
                            (-1) * totalCost, btcTotal', usdTotal')
    return ()

decreaseSmallTxFund :: BridgewalkerHandles -> EitherT String IO ()
decreaseSmallTxFund bwHandles = do
    let minimumOrderBTC = bcMtGoxMinimumOrderBTC . bhConfig $ bwHandles
        dbConn = bhDBConnPAT bwHandles
        mtgoxHandles = bhMtGoxHandles bwHandles
        logger = bhAppLogger bwHandles
    checkMtGoxWallet bwHandles 0
    btcTotal <- liftIO $ getSmallTxFundBTCTotal dbConn
    usdTotal <- liftIO $ getSmallTxFundUSDTotal dbConn
    mtgoxOrder <- liftIO $ callHTTPApi mtgoxHandles submitOrder
                                            OrderTypeSellBTC minimumOrderBTC
    orderStats <- case mtgoxOrder of
                    Left orderErr -> do
                        let logMsg = MtGoxError orderErr
                        liftIO $ logger logMsg
                        left mtgoxCommunicationErrorMsg
                    Right stats -> return stats
    let totalEarnings = usdEarned orderStats - usdFee orderStats
        btcTotal' = btcTotal - minimumOrderBTC
        usdTotal' = usdTotal + totalEarnings
        desc = "Sold " ++ formatBTCAmount minimumOrderBTC ++ " BTC for "
                    ++ formatUSDAmount totalEarnings ++ " USD; new totals are "
                    ++ formatBTCAmount btcTotal' ++ " BTC and "
                    ++ formatUSDAmount usdTotal' ++ " USD."
        logMsg = SmallTxFundAction { lcBTCChange = (-1) * minimumOrderBTC
                                   , lcUSDChange = totalEarnings
                                   , lcBTCTotal = btcTotal'
                                   , lcUSDTotal = usdTotal'
                                   , lcInfo = desc
                                   }
    liftIO $ logger logMsg
    liftIO . void $ execute dbConn "insert into small_tx_fund\
                        \ (timestamp, description, btc_change, usd_change\
                        \ , btc_total, usd_total)\
                        \ values (?, ?, ?, ?, ?, ?)"
                        ("Now" :: String, desc, (-1) * minimumOrderBTC,
                            totalEarnings, btcTotal', usdTotal')
    return ()

buyBTC :: BridgewalkerHandles-> BridgewalkerAccount -> QuoteData -> EitherT String IO Integer
buyBTC bwHandles bwAccount quoteData = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        logger = bhAppLogger bwHandles
        account = bAccount bwAccount
        targetFee = bcTargetExchangeFee . bhConfig $ bwHandles
        btcAmount = qdBTC quoteData
    mtgoxOrder <- liftIO $ callHTTPApi mtgoxHandles submitOrder
                                            OrderTypeBuyBTC btcAmount
    orderStats <- case mtgoxOrder of
                    Left orderErr -> do
                        let logMsg = MtGoxError orderErr
                        liftIO $ logger logMsg
                        left mtgoxCommunicationErrorMsg
                    Right stats -> return stats
    let extraFee = determineExtraFees orderStats targetFee
        totalCost = usdSpent orderStats + usdFee orderStats + extraFee
    let logMsg = BTCBought
                    { lcAccount = account
                    , lcUSDSpent = usdSpent orderStats
                    , lcUSDFee = usdFee orderStats
                    , lcUSDExtraFee = extraFee
                    , lcInfo = formatBTCAmount btcAmount
                                ++ " BTC bought on Mt.Gox for a total cost of "
                                ++ formatUSDAmount totalCost ++ " USD."
                    }
    liftIO $ logger logMsg
    debitAccount bwHandles bwAccount totalCost btcAmount

debitAccount :: BridgewalkerHandles-> BridgewalkerAccount -> Integer -> Integer -> EitherT String IO Integer
debitAccount bwHandles bwAccount totalCost btcAmount = do
    let logger = bhAppLogger bwHandles
        dbConn = bhDBConnPAT bwHandles
        account = bAccount bwAccount
    usdBalance <- liftIO $ getUSDBalance dbConn account
    let newUSDBalance = usdBalance - totalCost
    if newUSDBalance >= 0
        then do
            let info = "Account " ++ show account ++ " debited "
                        ++ formatUSDAmount totalCost ++ " USD for buying "
                        ++ formatBTCAmount btcAmount ++ " BTC"
                        ++ " - new balance is "
                        ++ formatUSDAmount newUSDBalance ++ " USD."
                logMsg' = AccountDebited
                            { lcAccount = account
                            , lcAmount = totalCost
                            , lcBalance = newUSDBalance
                            , lcInfo = info
                            }
            liftIO $ logger logMsg'
            liftIO . void $ execute dbConn "update accounts set usd_balance=?\
                                           \ where account_nr=?"
                                           (newUSDBalance, account)
            return btcAmount
        else do
            let fractionPayed = fromIntegral usdBalance
                                    / fromIntegral totalCost
                adjustedBtcAmount =
                    floor $ fractionPayed * fromIntegral btcAmount
                info = "Account " ++ show account
                        ++ " reached negative balance when paying "
                        ++ formatUSDAmount totalCost ++ " USD for "
                        ++ formatBTCAmount btcAmount ++ " BTC"
                        ++ " - account has been set to zero and only "
                        ++ formatBTCAmount adjustedBtcAmount ++ " BTC"
                        ++ " will be payed out."
                logMsg' = AccountOverdrawn
                            { lcAccount = account
                            , lcAmount = totalCost
                            , lcFractionPayed = fractionPayed
                            , lcBTCPayedOut = adjustedBtcAmount
                            , lcInfo = info
                            }
            liftIO $ logger logMsg'
            liftIO . void $ execute dbConn "update accounts set usd_balance=?\
                                           \ where account_nr=?"
                                           (0 :: Integer, account)
            return adjustedBtcAmount

determineExtraFees :: OrderStats -> Double -> Integer
determineExtraFees orderStats targetFee =
    let targetMarkup =
            round $ fromIntegral (usdSpent orderStats) * (targetFee / 100)
    in max 0 (targetMarkup - usdFee orderStats)


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
    checkOrderRange quoteData bwHandles
    checkMtGoxWallet bwHandles (qdUSDAccount quoteData)
    checkBitcoindWallet bwHandles (qdBTC quoteData)
    return ()

checkOrderRange :: QuoteData -> BridgewalkerHandles -> EitherT String IO ()
checkOrderRange quoteData bwHandles = do
    let maximalOrderBTC = bcMaximalOrderBTC . bhConfig $ bwHandles
        maximalOrderBTCStr = formatBTCAmount maximalOrderBTC ++ " BTC"
        btcAmount = qdBTC quoteData
    tryAssert ("Currently the maximal order size is "
                ++ maximalOrderBTCStr ++ ".") $ btcAmount <= maximalOrderBTC

checkMtGoxWallet :: BridgewalkerHandles -> Integer -> EitherT String IO ()
checkMtGoxWallet bwHandles neededUSDAmount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        safetyMarginUSD = bcSafetyMarginUSD . bhConfig $ bwHandles
    privateInfo <- noteT mtgoxCommunicationErrorMsg
                    . MaybeT $ callHTTPApi mtgoxHandles getPrivateInfoR
    tryAssert "The server's hot wallet is running low."
                (piUsdBalance privateInfo >= neededUSDAmount + safetyMarginUSD)
    return ()

checkBitcoindWallet :: BridgewalkerHandles -> Integer -> EitherT String IO ()
checkBitcoindWallet bwHandles neededBTCAmount = do
    let rpcAuth = bcRPCAuth . bhConfig $ bwHandles
        safetyMarginBTC = bcSafetyMarginBTC . bhConfig $ bwHandles
        watchdogLogger = bhWatchdogLogger bwHandles
    btcSystemBalance <- liftIO $ RPC.getBalanceR (Just watchdogLogger) rpcAuth
                                                     confsNeededForSending True
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

tryToExecuteSellOrder :: MtGoxAPIHandles-> Integer -> Integer -> IO (Either SellOrderProblem OrderStats)
tryToExecuteSellOrder mtgoxHandles safetyMarginBTC amount = runEitherT $ do
    privateInfo <- noteT (MtGoxCallError "Unable to call getPrivateInfoR.")
                    . MaybeT $ callHTTPApi mtgoxHandles getPrivateInfoR
    _ <- tryAssert MtGoxLowBalance
            (piBtcBalance privateInfo >= amount + safetyMarginBTC)
    EitherT $ adjustMtGoxError <$> callHTTPApi mtgoxHandles submitOrder
                                                    OrderTypeSellBTC amount
    -- returns order stats

adjustMtGoxError :: Either String b -> Either SellOrderProblem b
adjustMtGoxError (Left mtgoxErr) = Left (MtGoxCallError mtgoxErr)
adjustMtGoxError (Right result) = Right result
