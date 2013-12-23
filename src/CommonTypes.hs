-- This module is necessary to avoid cyclic imports in some cases.
-- Unfortunately it leaks more than is necessary.
{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module CommonTypes
    ( PendingActionsTrackerHandle(..)
    , PendingActionsState(..)
    , PendingOrder(..)
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
    , QuoteData(..)
    , PureQuoteData(..)
    , ExchangeStatus(..)
    , AmountType(..)
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
                                , pasBTCImbalance :: Integer
                                , pasLastOrder :: Maybe PendingOrder
                                }
                           deriving (Show, Generic)

data PendingOrder = PendingOrder { poOpCount :: Integer
                                 , poAdjustment :: Integer
                                 }
                    deriving (Show, Generic)

data BridgewalkerAccount = BridgewalkerAccount { bAccount :: Integer }
                           deriving (Generic, Show, Eq, Ord, Typeable)

data BridgewalkerAction = DepositAction { baAmount :: Integer
                                        , baAddress :: RPC.BitcoinAddress
                                        }
                        | ConvertBTCAcount { baAmount :: Integer
                                           , baAccount :: BridgewalkerAccount
                                           }
                        | MarketAction
                        | SendPaymentAction { baAccount :: BridgewalkerAccount
                                            , baRequestID :: Integer
                                            , baAddress :: RPC.BitcoinAddress
                                            , baAmountType :: AmountType
                                            , baExpiration :: UTCTime
                                            }
                        | PauseAction { baExpiration :: UTCTime }
                        | HeartbeatAction
                        deriving (Show, Generic)

data ClientHubAnswer = ForwardStatusToClient ClientStatus
                     | ForwardQuoteToClient Integer (Maybe QuoteData)
                     | ForwardSuccessfulSend
                            Integer (Maybe RPC.SerializedTransaction)
                     | ForwardFailedSend Integer T.Text
                     | SendPongToClient Integer
                     | CloseConnectionWithClient

data ExchangeStatus = ExchangeAvailable { esExchangeRate :: Integer }
                    | ExchangeUnavailable

data ClientHubCommand = RegisterClient { chcAccount :: BridgewalkerAccount
                                       , chcAnswerChan :: Chan ClientHubAnswer
                                       }
                      | RequestClientStatus { chcAccount ::
                                                    BridgewalkerAccount }
                      | RequestQuote { chcAccount :: BridgewalkerAccount
                                     , chcRequestID :: Integer
                                     , chcAmountType :: AmountType
                                     }
                      | SendPayment { chcAccount :: BridgewalkerAccount
                                    , chcRequestID :: Integer
                                    , chcAddress :: T.Text
                                    , chcAmountType :: AmountType
                                    }
                      | ReceivedPing { chcAccount :: BridgewalkerAccount }
                      | CheckTimeouts
                      | ExchangeUpdate { chcExchangeStatus :: ExchangeStatus }
                      | SignalPossibleBitcoinEvents
                      | SignalAccountUpdates { chcAccounts ::
                                                    [BridgewalkerAccount] }
                      | SignalSuccessfulSend
                            { chcAccount :: BridgewalkerAccount
                            , chcRequestID :: Integer
                            , chcSerializedTransactionM
                                :: Maybe RPC.SerializedTransaction
                            }
                      | SignalFailedSend
                            { chcAccount :: BridgewalkerAccount
                            , chcRequestID :: Integer
                            , chcReason :: T.Text
                            }

newtype ClientHubHandle = ClientHubHandle { unCHH :: Chan ClientHubCommand }

data ClientStatus = ClientStatus { csUSDBalance :: Integer
                                 , csBTCIn :: Integer
                                 , csPrimaryBTCAddress :: T.Text
                                 , csPendingTxs :: [ClientPendingTransaction]
                                 , csExchangeAvailable :: Bool
                                 , csExchangeRate :: Integer
                                 }
                    deriving (Show)

data AmountType = AmountBasedOnBTC Integer
                | AmountBasedOnUSDBeforeFees Integer
                | AmountBasedOnUSDAfterFees Integer
                deriving (Generic, Show)

instance Serialize AmountType

data QuoteData = QuoteData { qdBTC :: Integer
                           , qdUSDRecipient :: Integer
                           , qdUSDAccount :: Integer
                           , qdSufficientBalance :: Bool
                           , qdNeedsSmallTxFund :: Bool
                           }
                 deriving (Show)

data PureQuoteData = PureQuoteData { pqdBTC :: Integer
                                   , pqdUSDRecipient :: Integer
                                   , pqdUSDAccount :: Integer
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
                | LargeDeposit { lcAccount :: Integer
                               , lcInfo :: String
                               }
                | BTCConvertedToFiat { lcAccount :: Integer
                                     , lcInfo :: String
                                     , lcImbalance :: Integer
                                     }
                | FiatConvertedToBTC { lcAccount :: Integer
                                     , lcInfo :: String
                                     }
                | ImbalanceAdjustedAfterSending { lcInfo :: String
                                                , lcImbalance :: Integer
                                                }
                | OneShotSellOrderPlaced { lcInfo :: String
                                         , lcOpCount :: Integer
                                         , lcAmount :: Integer
                                         , lcImbalance :: Integer
                                         }
                | OneShotBuyOrderPlaced { lcInfo :: String
                                        , lcOpCount :: Integer
                                        , lcAmount :: Integer
                                        , lcImbalance :: Integer
                                        }
                | OneShotOrderProbablySuccessful { lcInfo :: String
                                                 , lcAdjustment :: Integer
                                                 , lcImbalance :: Integer
                                                 }
                | MtGoxLowBTCBalance { lcInfo :: String }
                | MtGoxLowUSDBalance { lcInfo :: String }
                | MtGoxError { lcInfo :: String }
                | MtGoxStillOpenOrders { lcInfo :: String }
                | MtGoxOrderBookUnavailable { lcInfo :: String }
                | MtGoxOrderBookInsufficient { lcInfo :: String }
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
                | SendPaymentFailedCheck { lcAccount :: Integer
                                         , lcAddress :: String
                                         , lcInfo :: String
                                         }
                | InternalTransfer { lcAccount :: Integer
                                   , lcOtherAccount :: Integer
                                   , lcAmount :: Integer
                                   , lcInfo :: String
                                   }
                | GuestAccountCreated { lcAccountName :: String }
                | UserLoggedIn { lcAccount :: Integer }
                | DisconnectedStaleClient
                    { lcClientsDisconnected :: Integer
                    , lcClientsRemaining :: Integer
                    , lcInfo :: String
                    }
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
            exchangeAvailable = csExchangeAvailable cs
            exchangeRate = csExchangeRate cs
        in object [ "usd_balance" .= usdBalance
                  , "btc_in" .= btcIn
                  , "primary_btc_address" .= primaryBTCAddress
                  , "pending_txs" .= pendingTxs
                  , "exchange_available" .= exchangeAvailable
                  , "exchange_rate" .= exchangeRate
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

instance Serialize PendingOrder

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
