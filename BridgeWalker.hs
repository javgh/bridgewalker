module Main where

import Network.MtGoxAPI

import Config
import LoggingUtils
import Rebalancer

main = do
    (rpcAuth, mtgoxCreds, safetyMargin) <- readConfig
    mtgoxHandles <- initMtGoxAPI Nothing mtgoxCreds
    appLogger <- initLogger
    rbHandle <- initRebalancer appLogger Nothing
                                    rpcAuth mtgoxHandles safetyMargin
    runRebalancer rbHandle
