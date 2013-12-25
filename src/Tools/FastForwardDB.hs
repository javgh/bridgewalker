{-# LANGUAGE OverloadedStrings #-}
module Tools.FastForwardDB
    ( fastForwardDB
    ) where

import Control.Monad
import Network.BitcoinRPC.Events.MarkerAddresses

import Config
import DbUtils

fastForwardDB :: BridgewalkerHandles -> IO ()
fastForwardDB bwHandles =
    let fbetHandle = bhFilteredBitcoinEventTaskHandle bwHandles
        dbConn = bhDBConnFBET bwHandles
        dbLock = bhDBWriteLock bwHandles
    in forever $ do
        (fetState, fEvents) <- waitForFilteredBitcoinEvents fbetHandle
        mapM_ print fEvents
        withSerialTransaction dbLock dbConn $
            writeBitcoindStateToDB dbConn fetState
