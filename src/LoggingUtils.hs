module LoggingUtils
    ( initLogging
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Time
import Network.Metricsd.Client
import System.Directory
import System.FilePath
import System.IO
import System.Locale

import CommonTypes

logDirectory :: String
logDirectory = "./log/"

logName :: String
logName = "bridgewalker"

linesPerFile :: Integer
linesPerFile = 100000

initLogging :: Bool -> IO (LoggingHandle, Logger)
initLogging copyToStdOut = do
    lHandle <- LoggingHandle <$> newChan
    _ <- forkIO $ loggerLoop lHandle copyToStdOut
    let logger = performLogging lHandle
    return (lHandle, logger)

loggerLoop :: LoggingHandle -> Bool -> IO ()
loggerLoop (LoggingHandle cmdChan) copyToStdOut = do
    mcHandle <- initMetricsdClient
    logfile <- openLogfile
    go mcHandle logfile 0
  where
    go mcHandle logfile count = do
        cmd <- readChan cmdChan
        case cmd of
            PerformLogging logContent -> do
                sendMetric mcHandle logContent
                when copyToStdOut $ logToStdOut logContent
                (logfile', count') <- logToFile logfile count logContent
                go mcHandle logfile' count'

sendMetric :: MetricsdClientHandle -> LogContent -> IO ()
sendMetric h RebalancerFailure{} = do
    sendMeter h "rebalancer.failures"
    sendMeter h "bridgewalker_errors"
sendMetric h RebalancerAction{} = sendMeter h "rebalancer.actions"
sendMetric h DepositProcessed{} = sendMeter h "transactions.incoming"
sendMetric h BTCSold{} = sendMeter h "exchange.btc_sold"
sendMetric h BTCBought{} = sendMeter h "exchange.btc_bought"
sendMetric h MtGoxError{} = do
    sendMeter h "exchange.errors"
    sendMeter h "bridgewalker_errors"
sendMetric h MtGoxLowBTCBalance{} = sendMeter h "low_balance"
sendMetric h BitcoindLowBTCBalance{} = sendMeter h "low_balance"
sendMetric h BTCSent{} = sendMeter h "transactions.outgoing"
sendMetric h BTCSendNetworkOrParseError{} = sendMeter h "bridgewalker_errors"
sendMetric h BTCSendError{} = sendMeter h "bridgewalker_errors"
sendMetric h SendPaymentFailedCheck{} =
    sendMeter h "send_payment_failed_checks"
sendMetric h SmallTxFundAction{} = sendMeter h "small_tx_fund.actions"
sendMetric h InternalTransfer{} = sendMeter h "internal_transfers"
sendMetric h GuestAccountCreated{} = sendMeter h "guest_accounts"
sendMetric h UserLoggedIn{} = sendMeter h "user_logins"
sendMetric h WatchdogError{} = sendMeter h "watchdog_errors"
sendMetric _ _ = return ()

logToStdOut :: LogContent -> IO ()
logToStdOut logContent = do
    now <- getCurrentTime
    putStrLn $ show now ++ "\t" ++ show logContent

logToFile :: Handle -> Integer -> LogContent -> IO (Handle, Integer)
logToFile logfile count logContent
  | count < linesPerFile = do
        now <- getCurrentTime
        let entry = LogEntry now logContent
        hPrint logfile entry
        hFlush logfile      -- TODO: what is the performance impact of this?
        return (logfile, count + 1)
  | otherwise = do
        hClose logfile
        logfile' <- openLogfile
        logToFile logfile' 0 logContent

openLogfile :: IO Handle
openLogfile = do
    exists <- doesDirectoryExist logDirectory
    unless exists $
        error "Logging error: log directory not found"
    timestamp <- formatTime defaultTimeLocale "%F_%H_%M_%S" <$> getCurrentTime
    let logfile = logDirectory </> logName ++ "." ++ timestamp ++ ".log"
    openFile logfile AppendMode

performLogging :: LoggingHandle -> LogContent -> IO ()
performLogging (LoggingHandle cmdChan) logConent = do
    writeChan cmdChan $ PerformLogging logConent
    return ()
