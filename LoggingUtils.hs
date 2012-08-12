{-# LANGUAGE DeriveGeneric #-}

module LoggingUtils
    ( initLogger
    , LogContent(..)
    ) where

import Control.Applicative
import Data.Serialize
import Data.Time
import GHC.Generics

data LogContent = LogMisc { lcInfo :: String }
                  deriving (Generic, Show)

data LogEntry = LogEntry { leTimestamp :: UTCTime
                         , leContent :: LogContent
                         }
                deriving (Generic, Show)

instance Serialize LogContent

instance Serialize UTCTime where
    put = put . show
    get = read <$> get

instance Serialize LogEntry

initLogger :: IO (LogContent -> IO ())
initLogger = return logger

logger :: LogContent -> IO ()
logger content = do
    now <- getCurrentTime
    let entry = LogEntry now content
    putStrLn $ show entry
