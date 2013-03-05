module PendingActionsTrackerQueueManagement
    ( addPendingActions
    , putPendingActions
    , addPendingAction
    , putPendingAction
    , popPendingAction
    , nudgePendingActionsTracker
    ) where

import Control.Concurrent
import Database.PostgreSQL.Simple
import Data.List

import qualified Data.Sequence as S

import CommonTypes
import DbUtils

nudgePendingActionsTracker :: PendingActionsTrackerHandle -> IO ()
nudgePendingActionsTracker (PendingActionsTrackerHandle chan) =
    writeChan chan ()

-- | Will use the provided database connection to add new actions
-- for the PendingActionsTracker. Important: This must be part of a database
-- transaction, so that reading and writing are one atomic operation.
addPendingActions :: Connection -> [BridgewalkerAction] -> IO ()
addPendingActions = modifyPendingActions addPendingAction

-- | Similar to 'addPendingActions', but adds the actions to the front of the
-- queue. Important: This must be part of a database transaction, so that
-- reading and writing are one atomic operation.
putPendingActions :: Connection -> [BridgewalkerAction] -> IO ()
putPendingActions = modifyPendingActions putPendingAction

modifyPendingActions :: (PendingActionsState -> BridgewalkerAction -> PendingActionsState)-> Connection -> [BridgewalkerAction] -> IO ()
modifyPendingActions f dbConn actions = do
    paState <- readPendingActionsStateFromDB dbConn
    let paState' = foldl' f paState actions
    writePendingActionsStateToDB dbConn paState'

popPendingAction :: PendingActionsState-> Maybe (BridgewalkerAction, PendingActionsState)
popPendingAction state =
    let pseq = pasSequence state
    in if S.null pseq
            then Nothing
            else let (a, as) = S.splitAt 1 pseq
                     action = S.index a 0   -- should always succeed as
                                            -- we checked that the sequence
                                            -- is not empty
                 in Just (action, state { pasSequence = as })

-- | Add a new action to the front of the pending actions queue.
putPendingAction :: PendingActionsState -> BridgewalkerAction -> PendingActionsState
putPendingAction state action =
    let pseq = pasSequence state
    in state { pasSequence = action S.<| pseq }

-- | Add a new action to the end of the pending actions queue.
addPendingAction :: PendingActionsState-> BridgewalkerAction -> PendingActionsState
addPendingAction state action =
    let pseq = pasSequence state
    in state { pasSequence = pseq S.|> action }
