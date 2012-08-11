module Main where

import Config
import Rebalancer
import Network.MtGoxAPI

main = do
    (rpcAuth, mtgoxCreds) <- readConfig
    mtgoxHandles <- initMtGoxAPI Nothing mtgoxCreds
    runRebalancer Nothing rpcAuth mtgoxHandles
