-- This module is necessary to avoid cyclic imports in some cases.
-- Unfortunately it leaks more than is necessary.
{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveDataTypeable #-}
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
    , confsNeededForSending
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Serialize
import Data.Time
import Data.Time.Clock
import Data.Typeable
import GHC.Generics

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

data ClientHubCommand = RegisterClient { chcAccount :: BridgewalkerAccount
                                       , chcAnswerChan :: Chan ClientHubAnswer
                                       }
                      | RequestClientStatus { chcAccount :: BridgewalkerAccount }
                      | SignalPossibleBitcoinEvents

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

confsNeededForSending :: Integer
confsNeededForSending = 3
