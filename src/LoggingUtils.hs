module LoggingUtils
    ( initLogging
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Time
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

initLogging :: IO (LoggingHandle, Logger)
initLogging = do
    lHandle <- LoggingHandle <$> newChan
    _ <- forkIO $ loggerLoop lHandle
    let logger = performLogging lHandle
    return (lHandle, logger)

loggerLoop :: LoggingHandle -> IO ()
loggerLoop (LoggingHandle cmdChan) = do
    logfile <- openLogfile
    go logfile 0
  where
    go logfile count = do
        cmd <- readChan cmdChan
        case cmd of
            PerformLogging logContent -> do
                (logfile', count') <- logToFile logfile count logContent
                go logfile' count'

logToFile :: Handle -> Integer -> LogContent -> IO (Handle, Integer)
logToFile logfile count logContent
  | count < linesPerFile = do
        now <- getCurrentTime
        let entry = LogEntry now logContent
        hPutStrLn logfile $ show entry
        hFlush logfile      -- TODO: what is the performance impact of this?
        putStrLn $ (show now) ++ "\t" ++ show logContent
        return (logfile, count + 1)
  | otherwise = do
        hClose logfile
        logfile' <- openLogfile
        logToFile logfile' 0 logContent

openLogfile :: IO Handle
openLogfile = do
    exists <- doesDirectoryExist logDirectory
    when (not exists) $
        error "Logging error: log directory not found"
    timestamp <- formatTime defaultTimeLocale "%F_%H_%M_%S" <$> getCurrentTime
    let logfile = logDirectory </> logName ++ "." ++ timestamp ++ ".log"
    openFile logfile AppendMode

performLogging :: LoggingHandle -> LogContent -> IO ()
performLogging (LoggingHandle cmdChan) logConent = do
    writeChan cmdChan $ PerformLogging logConent
    return ()
