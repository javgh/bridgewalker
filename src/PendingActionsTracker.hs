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
import Data.List
import Data.Time.Clock
import Network.MtGoxAPI
import Text.Printf
import Text.Regex

import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC
import qualified Data.Sequence as S

import AddressUtils
import CommonTypes
import Config
import DbUtils
import LoggingUtils
import PendingActionsTrackerQueueManagement
import QuoteUtils

import qualified ClientHub as CH

pauseInterval :: NominalDiffTime
pauseInterval = 60  -- pauses are 60 seconds long

mtgoxCommunicationErrorMsg = "Currently unable to communicate with Mt.Gox."

fatalSendPaymentError = "An untimely error has left your account in an\
                        \ inconsistent state. Please contact support.\
                        \ Sorry for the inconvenience!"

data PendingActionsStateModification = RemoveAction
                                     | ReplaceAction BridgewalkerAction
                                        -- this is the replacement action
                                     | KeepAction
                                     | AddPauseAction String
                                        -- parameter describes reason for pause

data WithdrawalType = WithdrawBTC { wtAmount :: Integer }
                    | WithdrawUSD { wtAmount :: Integer }
                    deriving (Show)

data WithdrawalAction = WithdrawalAction { waAddress :: RPC.BitcoinAddress
                                         , waType :: WithdrawalType
                                         }
                        deriving (Show)

data SellOrderProblem = MtGoxLowBalance | MtGoxCallError String

data SendPaymentAnswer = SendPaymentSuccessful
                            { spaAccount :: BridgewalkerAccount
                            , spaRequestID :: Integer
                            }
                       | SendPaymentFailed
                            { spaAccount :: BridgewalkerAccount
                            , spaRequestID :: Integer
                            , spaReason :: T.Text
                            }

initialPendingActionsState :: PendingActionsState
initialPendingActionsState = PendingActionsState { pasSequence = S.empty
                                                 , pasStatus = ""
                                                 }

initPendingActionsTracker :: BridgewalkerHandles -> IO PendingActionsTrackerHandle
initPendingActionsTracker bwHandles = do
    chan <- newChan
    forkIO $ trackerLoop bwHandles chan
    let handle = PendingActionsTrackerHandle chan
    nudgePendingActionsTracker handle
    return handle

trackerLoop :: BridgewalkerHandles -> Chan () -> IO ()
trackerLoop bwHandles chan =
    let dbConn = bhDBConnPAT bwHandles
        chHandle = bhClientHubHandle bwHandles
    in forever $ do
        _ <- readChan chan
        (touchedAccounts, sendPaymentAnswerM) <- withTransaction dbConn $ do
            paState <- readPendingActionsStateFromDB dbConn
            putStrLn $ "[PAT] >>>>>>>> Before processing: " ++ show paState
            (paState', keepGoing, touchedAccounts, sendPaymentAnswerM)
                <- maybeProcessOneAction bwHandles paState
            putStrLn $ "[PAT] >>>>>>>> After processing: " ++ show paState'
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
        BuyBTCAction amount address account ->
            buyBTC bwHandles amount address account
        SendBTCAction amount address account ->
            sendBTC bwHandles amount address account
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
            expiration <- addUTCTime pauseInterval <$> getCurrentTime                       -- IO: getCurrentTime
            let paState''' = putPendingAction paState'' $ PauseAction expiration
            return (paState''' { pasStatus = status }, False
                        , touchedAccounts, sendPaymentAnswerM)

checkPause :: UTCTime -> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
checkPause expiration = do
    now <- getCurrentTime                                                                   -- IO: getCurrentTime
    return $ if now >= expiration
                then (RemoveAction, [], Nothing)
                else (KeepAction, [], Nothing)

processDeposit :: BridgewalkerHandles-> Integer -> RPC.BitcoinAddress -> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
processDeposit bwHandles amount address = do
    let dbConn = bhDBConnPAT bwHandles
        logger = bhAppLogger bwHandles
        minimalOrderBTC = bcMtGoxMinimalOrderBTC . bhConfig $ bwHandles
    putStrLn "[PAT] in processDeposit function"
    accountM <- getAccountByAddress dbConn (adjustAddr address)                             -- IO: Database
    case accountM of
        Nothing -> do
            let logMsg = SystemDepositProcessed
                            { lcInfo = "Deposit to system -\
                                       \ no matching account found." }
            logger logMsg                                                                   -- IO: Logger
            return (RemoveAction, [], Nothing)
        Just (BridgewalkerAccount account) -> do
            btcBalance <- getBTCInBalance dbConn account                                    -- IO: Database
            let newBalance = btcBalance + amount
            execute dbConn                                                                  -- IO: Database
                        "update accounts set btc_in=? where account_nr=?"
                        (newBalance, account)
            let logMsg = DepositProcessed
                            { lcAccount = account
                            , lcInfo = show amount
                                        ++ " BTC deposited into account "
                                        ++ show account
                                        ++ " - balance is now "
                                        ++ show newBalance ++ " BTC."
                            }
            logger logMsg                                                                   -- IO: Logger
            let bwAccount = BridgewalkerAccount account
            return $ if newBalance >= minimalOrderBTC
                        then let action = SellBTCAction
                                            { baAmount = newBalance
                                            , baAccount = bwAccount
                                            }
                             in (ReplaceAction action, [bwAccount], Nothing)
                        else (RemoveAction, [bwAccount], Nothing)

sellBTC :: BridgewalkerHandles-> Integer-> BridgewalkerAccount-> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
sellBTC bwHandles amount bwAccount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        safetyMarginBTC = bcSafetyMarginBTC . bhConfig $ bwHandles
        logger = bhAppLogger bwHandles
        dbConn = bhDBConnPAT bwHandles
        account = bAccount $ bwAccount
    sell <- tryToExecuteSellOrder mtgoxHandles safetyMarginBTC amount                       -- IO: Mt.Gox
    case sell of
        Left (MtGoxCallError msg) -> do
            let logMsg = MtGoxError
                            { lcInfo = "Error communicating with Mt.Gox while\
                                       \ attempting to sell BTC. Error was: \
                                       \ " ++ msg }
            logger logMsg                                                                   -- IO: Logger
            return (AddPauseAction "Communication problems with Mt.Gox\
                                   \ - pausing until it is resolved."
                                   , [], Nothing)
        Left MtGoxLowBalance -> do
            let logMsg = MtGoxLowBTCBalance
                        { lcInfo = "Postponing BTC sell order because of low\
                                   \ balance in Mt.Gox account." }
            logger logMsg                                                                   -- IO: Logger
            return (AddPauseAction "Pausing until rebalancing of\
                                   \ reserves is completed.", [], Nothing)
        Right stats -> do
            let usdAmount = max 0 (usdEarned stats - usdFee stats)
            btcBalance <- getBTCInBalance dbConn account                                    -- IO: Database
            usdBalance <- getUSDBalance dbConn account                                      -- IO: Database
            let newBTCBalance = max 0 (btcBalance - amount)
                newUSDBalance = usdBalance + usdAmount
            execute dbConn "update accounts set btc_in=?, usd_balance=?\
                                \ where account_nr=?"
                                (newBTCBalance, newUSDBalance, account)                     -- IO: Database
            let logMsg = BTCSold
                            { lcAccount = account
                            , lcInfo = show amount
                                        ++ " BTC sold on Mt.Gox and credited "
                                        ++ show usdAmount ++ " USD to account "
                                        ++ show account ++ " - balance is now "
                                        ++ show newUSDBalance ++ " USD and "
                                        ++ show newBTCBalance ++ " BTC."
                            }
            logger logMsg                                                                   -- IO: Logger
            return (RemoveAction, [BridgewalkerAccount account], Nothing)

-- TODO: add check for other Bridgewalker user
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
        Right _ ->
            let answer = SendPaymentSuccessful account requestID
            in return (RemoveAction, [account], Just answer)
  where
    go = do
            now <- liftIO getCurrentTime
            tryAssert busyMsg (now < expiration)
            quoteData <- sendPaymentPreparationChecks bwHandles account
                                                            address amountType
            tryAssert busyMsg (now < expiration)    -- check again, as some
                                                    -- previous checks might
                                                    -- have blocked for a while
            btcAmountToSend <- buyBTC' bwHandles account quoteData
            txID <- sendBTC' bwHandles account address btcAmountToSend
            return txID
    busyMsg = "The server is very busy at the moment. Please try again later."

sendBTC' :: BridgewalkerHandles-> BridgewalkerAccount-> RPC.BitcoinAddress-> Integer-> EitherT String IO RPC.TransactionID
sendBTC' bwHandles bwAccount address btcAmountToSend = do
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

buyBTC' :: BridgewalkerHandles-> BridgewalkerAccount -> QuoteData -> EitherT String IO Integer
buyBTC' bwHandles bwAccount quoteData = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        logger = bhAppLogger bwHandles
        dbConn = bhDBConnPAT bwHandles
        account = bAccount bwAccount
        targetFee = bcTargetExchangeFee . bhConfig $ bwHandles
        btcAmount = qdBTC quoteData
    mtgoxOrder <- liftIO $ callHTTPApi mtgoxHandles submitOrder
                                            OrderTypeBuyBTC btcAmount
    orderStats <- case mtgoxOrder of
                    Left err -> do
                        let logMsg = MtGoxError err
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
    usdBalance <- liftIO $ getUSDBalance dbConn account
    let newUSDBalance = usdBalance - totalCost
    btcAmountToSend
        <- if newUSDBalance >= 0
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
                liftIO $ execute dbConn "update accounts set usd_balance=?\
                                        \ where account_nr=?"
                                        (newUSDBalance, account)
                return btcAmount
            else do
                undefined
    return btcAmountToSend

determineExtraFees :: OrderStats -> Double -> Integer
determineExtraFees orderStats targetFee =
    let targetMarkup =
            round $ fromIntegral (usdSpent orderStats) * (targetFee / 100)
    in max 0 (targetMarkup - usdFee orderStats)


sendPaymentPreparationChecks :: BridgewalkerHandles-> BridgewalkerAccount-> RPC.BitcoinAddress-> AmountType-> EitherT String IO QuoteData
sendPaymentPreparationChecks bwHandles account address amountType = do
    validatedAddress <- checkAddress bwHandles address
    qc <- liftIO $ compileQuote bwHandles account amountType
    quoteData <- case qc of
                    SuccessfulQuote qd -> return qd
                    HadNotEnoughDepth -> left "The entered amount is too large."
                    DepthStoreWasUnavailable -> left mtgoxCommunicationErrorMsg
    tryAssert "Insufficient funds to complete the transaction." $
                    qdSufficientBalance quoteData
    checkOrderRange quoteData bwHandles
    checkMtGoxWallet bwHandles (qdUSDAccount quoteData)
    checkBitcoindWallet bwHandles (qdBTC quoteData)
    return quoteData

checkOrderRange :: QuoteData -> BridgewalkerHandles -> EitherT String IO ()
checkOrderRange quoteData bwHandles = do
    let maximalOrderBTC = bcMaximalOrderBTC . bhConfig $ bwHandles
        minimalOrderBTC = bcMtGoxMinimalOrderBTC . bhConfig $ bwHandles
        minimalOrderBTCStr = formatBTCAmount minimalOrderBTC ++ " BTC"
        maximalOrderBTCStr = formatBTCAmount maximalOrderBTC ++ " BTC"
        btcAmount = qdBTC quoteData
    tryAssert ("Currently the minimal order size is "
                ++ minimalOrderBTCStr ++ ".") $ btcAmount >= minimalOrderBTC
    tryAssert ("Currently the maximal order size is "
                ++ maximalOrderBTCStr ++ ".") $ btcAmount <= maximalOrderBTC

formatBTCAmount :: Integer -> String
formatBTCAmount a =
    let a' = fromIntegral a / 10 ^ (8 :: Integer) :: Double
        str = printf "%.8f" a'
    in subRegex (mkRegex "\\.?0+$") str ""

formatUSDAmount :: Integer -> String
formatUSDAmount a =
    let a' = fromIntegral a / 10 ^ (5 :: Integer) :: Double
        str = printf "%.5f" a'
    in subRegex (mkRegex "\\.?0+$") str ""

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


buyBTC :: BridgewalkerHandles-> Integer-> RPC.BitcoinAddress-> BridgewalkerAccount-> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
buyBTC bwHandles amount address bwAccount = do
    let mtgoxHandles = bhMtGoxHandles bwHandles
        depthStoreHandle = mtgoxDepthStoreHandle mtgoxHandles
        safetyMarginUSD = bcSafetyMarginUSD . bhConfig $ bwHandles
        logger = bhAppLogger bwHandles
        dbConn = bhDBConnPAT bwHandles
        account = bAccount $ bwAccount
    -- TODO: check for minimal BTC amount
    -- TODO: check for sane amount
    usdAmountM <- simulateBTCBuy depthStoreHandle amount                                    -- IO: DepthStore
    case usdAmountM of
        DepthStoreUnavailable -> do
            let logMsg = MtGoxError
                            { lcInfo = "Stale data from depth store while\
                                       \ trying to calculate USD value of\
                                       \ BTC buy order." }
            logger logMsg                                                                   -- IO: ...
            return (AddPauseAction "Communication problems with Mt.Gox\
                                   \ - pausing until it is resolved."
                                   , [], Nothing)
        NotEnoughDepth -> return (RemoveAction, [], Nothing) -- TODO: maybe log something
        DepthStoreAnswer usdAmount -> do
            usdBalance <- getUSDBalance dbConn account
            -- TODO: take fees into account
            if usdAmount > usdBalance
                then return (RemoveAction, [], Nothing) -- TODO: figure out way in which to inform user;
                                                        --       probably via transaction log
                else do
                    buy <- tryToExecuteBuyOrder mtgoxHandles safetyMarginUSD
                                amount usdAmount
                    processBuyResult logger dbConn bwAccount amount address buy

processBuyResult :: Logger-> Connection-> BridgewalkerAccount-> Integer-> RPC.BitcoinAddress-> Either SellOrderProblem OrderStats-> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
processBuyResult logger dbConn bwAccount btcAmount address buy = do
    let account = bAccount $ bwAccount
    case buy of
        Left (MtGoxCallError msg) -> do
            let logMsg = MtGoxError
                            { lcInfo = "Error communicating with Mt.Gox while\
                                       \ attempting to buy BTC. Error was: \
                                       \ " ++ msg }
            logger logMsg
            return (AddPauseAction "Communication problems with Mt.Gox\
                                   \ - pausing until it is resolved."
                                   , [], Nothing)
        Left MtGoxLowBalance -> do
            let logMsg = MtGoxLowBTCBalance
                        { lcInfo = "Postponing BTC buy order because of low\
                                   \ balance in Mt.Gox account." }
            logger logMsg
            return (AddPauseAction "Hot wallet is running low - pausing\
                                   \ until resolved via cold reserves."
                                   , [], Nothing)
        Right stats -> do
            let usdAmount = usdSpent stats + usdFee stats
            usdBalance <- getUSDBalance dbConn account
            -- TODO: deal with case when usdAmount does end up lower
            --       than usdBalance, because things changed last minute
            let newUSDBalance = max 0 (usdBalance - usdAmount)
            execute dbConn "update accounts set usd_balance=?\
                                \ where account_nr=?"
                                (newUSDBalance, account)
--            let logMsg = BTCBought
--                            { lcAccount = account
--                            , lcInfo = show btcAmount
--                                        ++ " BTC bought on Mt.Gox and subtracted "
--                                        ++ show usdAmount ++ " USD from account "
--                                        ++ show account ++ " - balance is now "
--                                        ++ show newUSDBalance ++ " USD."
--                            }
--            logger logMsg
            let action = SendBTCAction { baAmount = btcAmount
                                       , CommonTypes.baAddress
                                            = address
                                       , baAccount = bwAccount
                                       }
            return (ReplaceAction action, [bwAccount], Nothing)

sendBTC :: BridgewalkerHandles-> Integer-> RPC.BitcoinAddress-> BridgewalkerAccount-> IO (PendingActionsStateModification, [BridgewalkerAccount], Maybe SendPaymentAnswer)
sendBTC bwHandles amount address bwAccount = do
    let rpcAuth = bcRPCAuth . bhConfig $ bwHandles
        safetyMarginBTC = bcSafetyMarginBTC . bhConfig $ bwHandles
        logger = bhAppLogger bwHandles
        watchdogLogger = bhWatchdogLogger bwHandles
        account = bAccount $ bwAccount
    btcSystemBalance <- RPC.getBalanceR (Just watchdogLogger) rpcAuth
                                            confsNeededForSending True
    if (amount + safetyMarginBTC <= adjustAmount btcSystemBalance)
        then do
            result <- RPC.sendToAddress rpcAuth address (adjustAmount amount)
            case result of
                Left networkOrParseError -> do
                    let logMsg = BTCSendNetworkOrParseError
                                    { lcAccount = account
                                    , lcAddress =
                                        T.unpack . RPC.btcAddress $ address
                                    , lcAmount = amount
                                    , lcInfo = networkOrParseError
                                    }
                    logger logMsg
                    return (RemoveAction, [bwAccount], Nothing)
                Right result' -> do
                    case result' of
                        Left sendError -> do
                            let logMsg = BTCSendError
                                            { lcAccount = account
                                            , lcAddress =
                                                T.unpack . RPC.btcAddress
                                                                    $ address
                                            , lcAmount = amount
                                            , lcInfo = show sendError
                                            }
                            logger logMsg
                            return (RemoveAction, [bwAccount], Nothing)
                        Right txID -> do
                            let logMsg = BTCSent
                                            { lcAccount = account
                                            , lcInfo = "Account "
                                                ++ show account ++ " sent out "
                                                ++ show amount ++ " BTC to "
                                                ++ (T.unpack . RPC.btcAddress
                                                                    $ address)
                                                ++ " with transaction "
                                                ++ (T.unpack . RPC.btcTxID
                                                                    $ txID)
                                                ++ "."
                                            }
                            logger logMsg
                            return (RemoveAction, [bwAccount], Nothing)
        else do
            let logMsg = BitcoindLowBTCBalance
                        { lcInfo = "Postponing BTC send action because of low\
                                   \ local BTC balance." }
            logger logMsg
            return (AddPauseAction "Pausing until rebalancing of\
                                   \ reserves is completed.", [], Nothing)

tryToExecuteSellOrder :: MtGoxAPIHandles-> Integer -> Integer -> IO (Either SellOrderProblem OrderStats)
tryToExecuteSellOrder mtgoxHandles safetyMarginBTC amount = runEitherT $ do
    privateInfo <- noteT (MtGoxCallError "Unable to call getPrivateInfoR.")
                    . MaybeT $ callHTTPApi mtgoxHandles getPrivateInfoR
    _ <- tryAssert MtGoxLowBalance
            (piBtcBalance privateInfo >= amount + safetyMarginBTC)
    orderStats <- EitherT $
        adjustMtGoxError <$> callHTTPApi mtgoxHandles submitOrder
                                OrderTypeSellBTC amount
    return orderStats

tryToExecuteBuyOrder :: MtGoxAPIHandles-> Integer-> Integer-> Integer-> IO (Either SellOrderProblem OrderStats)
tryToExecuteBuyOrder mtgoxHandles safetyMarginUSD amountBTC amountUSD = runEitherT $ do
    privateInfo <- noteT (MtGoxCallError "Unable to call getPrivateInfoR.")
                    . MaybeT $ callHTTPApi mtgoxHandles getPrivateInfoR
    _ <- tryAssert MtGoxLowBalance
            (piUsdBalance privateInfo >= amountUSD + safetyMarginUSD)
    orderStats <- EitherT $
        adjustMtGoxError <$> callHTTPApi mtgoxHandles submitOrder
                                OrderTypeBuyBTC amountBTC
    return orderStats

adjustMtGoxError (Left err) = Left (MtGoxCallError err)
adjustMtGoxError (Right result) = Right result
