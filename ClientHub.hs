{-# LANGUAGE OverloadedStrings #-}
module ClientHub
    ( ClientStatus
    , compileClientStatus
    ) where

import Control.Concurrent
import Data.Aeson

import qualified Data.Text as T
import qualified Network.BitcoinRPC as RPC
import qualified Network.BitcoinRPC.Events.MarkerAddresses as MA

import CommonTypes
import Config
import DbUtils

data ClientStatus = ClientStatus { csUSDBalance :: Integer
                                 , csBTCIn :: Integer
                                 , csPendingTxs :: [ClientPendingTransaction]
                                 }
                    deriving (Show)

data ClientPendingTransaction = ClientPendingTransaction
                                    { cptAmount :: Integer
                                    , cptReason :: ClientPendingReason
                                    }
                                deriving (Show)

data ClientPendingReason = TooFewConfirmations { cprConfs :: Integer }
                         | MarkerAddressLimitReached
                                { cprMarkerAddress :: T.Text }
                         deriving (Show)

instance ToJSON ClientPendingReason
  where
    toJSON (TooFewConfirmations confs) =
        object [ "type" .= ("too_few_confirmations" :: T.Text)
               , "confirmations" .= confs
               ]
    toJSON (MarkerAddressLimitReached markerAddress) =
        object [ "type" .= ("marker_address_limit_reached" :: T.Text)
               , "marker_address" .= markerAddress
               ]

instance ToJSON ClientPendingTransaction
  where
    toJSON cpt@ClientPendingTransaction{} =
        let amount = cptAmount cpt
            reason = cptReason cpt
        in object [ "amount" .= amount
                  , "reason" .= reason
                  ]

instance ToJSON ClientStatus
  where
    toJSON cs@ClientStatus{} =
        let usdBalance = csUSDBalance cs
            btcIn = csBTCIn cs
            pendingTxs = csPendingTxs cs
        in object [ "usd_balance" .= usdBalance
                  , "btc_in" .= btcIn
                  , "pending_txs" .= pendingTxs
                  ]

magicAddress = RPC.BitcoinAddress "17cWnmBb4b8EMrHhSiasMXsbsc1ru7iTGj"

compileClientStatus :: BridgewalkerHandles -> BridgewalkerAccount -> IO ClientStatus
compileClientStatus bwHandles bwAccount = do
    let dbConn = bhDBConn bwHandles
        fetStateCopy = bhFilteredEventStateCopy bwHandles
        account = bAccount bwAccount
    (btcIn, usdBalance) <- getClientDBStatus dbConn account
    fetState <- readMVar fetStateCopy
    let pendingTxs = map translatePendingTx
                        . filter ((==) magicAddress . RPC.tAddress . fst)
                        . MA.listPendingTransactions $ fetState
    let status = ClientStatus { csUSDBalance = usdBalance
                              , csBTCIn = btcIn
                              , csPendingTxs = pendingTxs
                              }
    return status
  where
    translatePendingTx (tx, reason) =
        let amount = RPC.btcAmount . RPC.tAmount $ tx
            cReason = case reason of
                        MA.TooFewConfirmations confs ->
                            TooFewConfirmations confs
                        MA.MarkerAddressLimitReached markerAddress ->
                            MarkerAddressLimitReached (RPC.btcAddress
                                                            markerAddress)
        in ClientPendingTransaction amount cReason
