-- This module is necessary to avoid cyclic imports in some cases.
-- Unfortunately it leaks more than is necessary.
{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}
module CommonTypes
    ( PendingActionsTrackerHandle(..)
    , PendingActionsState(..)
    , BridgewalkerAccount(..)
    , BridgewalkerAction(..)
    , ClientHubHandle(..)
    , ClientHubCommand(..)
    , ClientStatus(..)
    , ClientPendingTransaction(..)
    , ClientPendingReason(..)
    , ClientHubAnswer(..)
    , LoggingHandle(..)
    , Logger
    , LoggingCmd(..)
    , LogContent(..)
    , LogEntry(..)
    , confsNeededForSending
    , SnapApp(..)
    , SnapAppHandler
    , heist
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Serialize
import Data.Time
import Data.Typeable
import GHC.Generics
import Snap.Snaplet
import Snap.Snaplet.Heist

import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC

newtype PendingActionsTrackerHandle = PendingActionsTrackerHandle
                                        { unPATH :: Chan () }

data PendingActionsState = PendingActionsState
                                { pasSequence :: S.Seq BridgewalkerAction
                                , pasStatus :: String
                                }
                           deriving (Show, Generic)

data BridgewalkerAccount = BridgewalkerAccount { bAccount :: Integer }
                           deriving (Generic, Show, Eq, Ord, Typeable)

data BridgewalkerAction = DepositAction { baAmount :: Integer
                                        , baAddress :: RPC.BitcoinAddress
                                        }
                        | SellBTCAction { baAmount :: Integer
                                        , baAccount :: BridgewalkerAccount
                                        }
                        | BuyBTCAction { baAmount :: Integer
                                       , baAddress :: RPC.BitcoinAddress
                                       , baAccount :: BridgewalkerAccount
                                       }
                        | SendBTCAction { baAmount :: Integer
                                        , baAddress :: RPC.BitcoinAddress
                                        , baAccount :: BridgewalkerAccount
                                        }
                        | PauseAction { baExpiration :: UTCTime }
                        -- TODO: more actions
                        deriving (Show, Generic)

data ClientHubAnswer = ForwardStatusToClient ClientStatus
                     | SendPongToClient
                     | CloseConnectionWithClient

data ClientHubCommand = RegisterClient { chcAccount :: BridgewalkerAccount
                                       , chcAnswerChan :: Chan ClientHubAnswer
                                       }
                      | RequestClientStatus { chcAccount ::
                                                    BridgewalkerAccount }
                      | ReceivedPing { chcAccount :: BridgewalkerAccount }
                      | CheckTimeouts
                      | SignalPossibleBitcoinEvents
                      | SignalAccountUpdates { chcAccounts ::
                                                    [BridgewalkerAccount] }

newtype ClientHubHandle = ClientHubHandle { unCHH :: Chan ClientHubCommand }

data ClientStatus = ClientStatus { csUSDBalance :: Integer
                                 , csBTCIn :: Integer
                                 , csPrimaryBTCAddress :: T.Text
                                 , csPendingTxs :: [ClientPendingTransaction]
                                 }
                    deriving (Show)

data ClientPendingTransaction = ClientPendingTransaction
                                    { cptAmount :: Integer
                                    , cptReason :: ClientPendingReason
                                    }
                                deriving (Show, Eq)

data ClientPendingReason = TooFewConfirmations { cprConfs :: Integer }
                         | MarkerAddressLimitReached
                                { cprMarkerAddress :: T.Text }
                         deriving (Show, Eq)

data LoggingCmd = PerformLogging LogContent

newtype LoggingHandle = LoggingHandle { unLH :: Chan LoggingCmd }

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
                | UserLoggedIn { lcAccount :: Integer }
                | WatchdogError { lcInfo :: String }
                | LogMisc { lcInfo :: String }
                deriving (Generic, Show)

data LogEntry = LogEntry { _leTimestamp :: UTCTime
                         , _leContent :: LogContent
                         }
                deriving (Generic, Show)

instance Serialize LogContent

instance Serialize LogEntry

instance ToJSON ClientPendingReason where
    toJSON (TooFewConfirmations confs) =
        object [ "type" .= ("too_few_confirmations" :: T.Text)
               , "confirmations" .= confs
               ]
    toJSON (MarkerAddressLimitReached markerAddress) =
        object [ "type" .= ("marker_address_limit_reached" :: T.Text)
               , "marker_address" .= markerAddress
               ]

instance ToJSON ClientPendingTransaction where
    toJSON cpt@ClientPendingTransaction{} =
        let amount = cptAmount cpt
            reason = cptReason cpt
        in object [ "amount" .= amount
                  , "reason" .= reason
                  ]

instance ToJSON ClientStatus where
    toJSON cs@ClientStatus{} =
        let usdBalance = csUSDBalance cs
            btcIn = csBTCIn cs
            pendingTxs = csPendingTxs cs
            primaryBTCAddress = csPrimaryBTCAddress cs
        in object [ "usd_balance" .= usdBalance
                  , "btc_in" .= btcIn
                  , "primary_btc_address" .= primaryBTCAddress
                  , "pending_txs" .= pendingTxs
                  ]

instance Serialize Day where
    put = put . toModifiedJulianDay
    get = ModifiedJulianDay <$> get

instance Serialize DiffTime where
    put = put . toRational
    get = fromRational <$> get

instance Serialize UTCTime where
    put timestamp = put (utctDay timestamp) >> put (utctDayTime timestamp)
    get = liftM2 UTCTime get get

instance Serialize BridgewalkerAccount

instance Serialize BridgewalkerAction

instance Serialize PendingActionsState

data SnapApp = SnapApp
    { _heist :: Snaplet (Heist SnapApp)
    }

makeLenses ''SnapApp

instance HasHeist SnapApp where
    heistLens = subSnaplet heist

type SnapAppHandler = Handler SnapApp SnapApp

confsNeededForSending :: Integer
confsNeededForSending = 3
