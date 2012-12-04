{-# LANGUAGE DeriveGeneric #-}

module LoggingUtils
    ( initLogger
    , LogContent(..)
    , Logger
    ) where

import Control.Applicative
import Data.Serialize
import Data.Time
import GHC.Generics

import CommonTypes

type Logger = LogContent -> IO ()

data LogContent = RebalancerFailure { lcInfo :: String }
                | RebalancerStatus { lcRLevel :: Integer
                                   , lcRWillAct :: Bool
                                   , lcInfo :: String
                                   }
                | RebalancerAction { lcInfo :: String }
                | DepositProcessed { lcAccount :: Integer
                                   , lcInfo :: String
                                   }
                | SystemDepositProcessed { lcInfo :: String }
                | BTCSold { lcAccount :: Integer
                          , lcInfo :: String
                          }
                | BTCBought { lcAccount :: Integer
                            , lcInfo :: String
                            }
                | MtGoxLowBTCBalance { lcInfo :: String }
                | MtGoxError { lcInfo :: String }
                | BitcoindLowBTCBalance { lcInfo :: String }
                | BTCSent { lcAccount :: Integer
                          , lcInfo :: String
                          }
                | BTCSendNetworkOrParseError
                    { lcAccount :: Integer
                    , lcAddress :: String
                    , lcAmount :: Integer
                    , lcInfo :: String
                    }
                | BTCSendError { lcAccount :: Integer
                               , lcAddress :: String
                               , lcAmount :: Integer
                               , lcInfo :: String
                               }
                | GuestAccountCreated { lcAccountName :: String }
                | WatchdogError { lcInfo :: String }
                | LogMisc { lcInfo :: String }
                deriving (Generic, Show)

data LogEntry = LogEntry { _leTimestamp :: UTCTime
                         , _leContent :: LogContent
                         }
                deriving (Generic, Show)

instance Serialize LogContent

instance Serialize LogEntry

initLogger :: IO Logger
initLogger = return logger

logger :: Logger
logger content = do
    now <- getCurrentTime
    let entry = LogEntry now content
    print entry
