module Utils
    ( linkedForkIO
    , whenJust
    ) where

import Control.Concurrent.Async

-- | Sparks off a new thread like 'forkIO', but links it with the current
-- thread, such that if the new thread raises an exception, that exception will
-- be re-thrown in the current thread.
linkedForkIO :: IO a -> IO ()
linkedForkIO action = async action >>= link

-- | Do something with a value inside a 'Maybe', unless it is 'Nothing'
-- in which case do nothing.
whenJust :: Monad m => Maybe t -> (t -> m ()) -> m ()
whenJust vM action = case vM of
                        Just v -> action v
                        Nothing -> return ()
