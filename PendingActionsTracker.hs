{-# LANGUAGE DeriveGeneric #-}
module PendingActionsTracker
    ( PendingActionsTrackerState
    , BridgewalkerAccount(..)
    , BridgewalkerAction(..)
    ) where

import Control.Applicative
import Data.Serialize
import GHC.Generics

import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC

data PendingActionsTrackerState = PendingActionsTrackerState
                                    { patsList :: [BridgewalkerAction] }
                                  deriving (Show, Generic)

data BridgewalkerAccount = BridgewalkerAccount { bAccount :: T.Text }
                           deriving (Show)

data WithdrawalType = WithdrawBTC { wtAmount :: Integer }
                    | WithdrawUSD { wtAmount :: Integer }
                    deriving (Show)

data WithdrawalAction = WithdrawalAction { waAddress :: RPC.BitcoinAddress
                                         , waType :: WithdrawalType
                                         }
                        deriving (Show)

data BridgewalkerAction = DepositAction { baAmount :: Integer
                                        , baAddress :: RPC.BitcoinAddress
                                        }
                        | SellBTCAction { baAmount :: Integer
                                        , baAccount :: BridgewalkerAccount
                                        }
                        -- TODO: more actions
                        deriving (Show, Generic)

instance Serialize BridgewalkerAccount where
    put = put . T.unpack . bAccount
    get = BridgewalkerAccount . T.pack <$> get

instance Serialize BridgewalkerAction

instance Serialize PendingActionsTrackerState

initPendingActionsTracker :: PendingActionsTrackerState
initPendingActionsTracker = PendingActionsTrackerState []

addPendingActions :: PendingActionsTrackerState-> BridgewalkerAction -> PendingActionsTrackerState
addPendingActions state action =
    let list = patsList state
    in PendingActionsTrackerState $ action : list
