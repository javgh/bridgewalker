-- This module is necessary to avoid cyclic imports in some cases.
-- Unfortunately it leaks more than is necessary.
{-# LANGUAGE DeriveGeneric #-}
module CommonTypes
    ( PendingActionsTrackerHandle(..)
    , PendingActionsState(..)
    , BridgewalkerAccount(..)
    , BridgewalkerAction(..)
    , confsNeededForSending
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Serialize
import Data.Time
import Data.Time.Clock
import GHC.Generics

import qualified Data.Sequence as S
import qualified Network.BitcoinRPC as RPC

newtype PendingActionsTrackerHandle = PendingActionsTrackerHandle
                                        { unPATH :: Chan () }

data PendingActionsState = PendingActionsState
                                { pasSequence :: S.Seq BridgewalkerAction
                                , pasStatus :: String
                                }
                           deriving (Show, Generic)

data BridgewalkerAccount = BridgewalkerAccount { bAccount :: Integer }
                           deriving (Generic, Show)

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
