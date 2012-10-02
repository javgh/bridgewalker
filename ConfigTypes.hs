-- This module is necessary to avoid some cyclic imports regarding the Config
-- module. Unfortunately it leaks more than is necessary.
module ConfigTypes
    ( PendingActionsTrackerHandle(..)
    ) where

import Control.Concurrent

newtype PendingActionsTrackerHandle = PendingActionsTrackerHandle
                                        { unPATH :: Chan () }
