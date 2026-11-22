{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module Network.Wai.Handler.Warp.Counter (
    Counter,
    newCounter,
    HasDecreased (..),
    waitForZero,
    waitForZeroFor,
    forkConnection,
    waitForDecreased,
    getCount,
    getCountSTM,
    threadManager,
) where

import Control.Concurrent.STM
import Control.Monad (when)
import Control.Exception (onException)
import Data.IORef (atomicModifyIORef', newIORef)
import System.ThreadManager (
    ThreadManager,
    countManaged,
    forkManagedByFinally,
    newThreadManager,
    waitUntilAllGone,
    waitUntilAllGoneFor,
 )

-- | The connections of a server: how many there are, and which threads
--   are serving them.
--
--   Both come from the 'ThreadManager', which counts a connection from
--   when the accept loop keeps a place for it -- before the thread that
--   will serve it has been scheduled, which is too late for a graceful
--   shutdown to notice it.
newtype Counter = Counter ThreadManager

newCounter :: IO Counter
newCounter = Counter <$> newThreadManager

-- | The manager owning the connections, for supervising them.
threadManager :: Counter -> ThreadManager
threadManager (Counter tm) = tm

waitForZero :: Counter -> IO ()
waitForZero (Counter tm) = waitUntilAllGone tm

-- | 'waitForZero' for at most the given microseconds, saying whether the
--   connections are gone.
waitForZeroFor :: Counter -> Int -> IO Bool
waitForZeroFor (Counter tm) = waitUntilAllGoneFor tm

data HasDecreased = HasDecreased | NoConnections | StillOpen
    deriving (Eq, Show)

-- | Waiting for a connection to end, for a second at most: the accept
--   loop waiting here looks again then, at whether it has been told to
--   stop among the rest.  With none to wait for, it waits the second.
waitForDecreased :: Counter -> IO HasDecreased
waitForDecreased (Counter tm) = do
    n0 <- atomically $ countManaged tm
    late <- registerDelay 1_000_000
    let expired = readTVar late >>= check
    if n0 <= 0
        then NoConnections <$ atomically expired
        else atomically $
            ( do
                n <- countManaged tm
                check (n < n0)
                pure HasDecreased
            )
                `orElse` (StillOpen <$ expired)

-- | Serving a connection on a thread made by the given fork.  The
--   connection is counted from here rather than from when that thread
--   starts: the accept loop does not wait for it, and a connection which
--   has been accepted must not look to 'waitForZero' like one that is
--   gone.
forkConnection
    :: Counter
    -> (((forall x. IO x -> IO x) -> IO ()) -> IO ())
    -- ^ The fork from the settings
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Serving the connection
    -> IO ()
    -- ^ Run instead when the thread never serves it: the fork failing, or
    --   a manager stopped before the thread could be recorded
    -> IO ()
forkConnection (Counter tm) fork io refused = do
    -- Whoever claims it first, the thread to serve or a way out without
    -- it to give up on it, is the only one to do so.
    claimed <- newIORef False
    let claim = atomicModifyIORef' claimed $ \b -> (True, not b)
        giveUp = claim >>= \won -> when won refused
    forkManagedByFinally
        tm
        fork
        "Warp just forked"
        (\unmask -> claim >>= \won -> when won $ io unmask)
        giveUp
        `onException` giveUp

-- | Get the current count of open connections.
--
-- @since 3.4.11
getCount :: Counter -> IO Int
getCount = atomically . getCountSTM

-- | Get the current count in an 'STM' transaction.
--
-- @since 3.4.13
getCountSTM :: Counter -> STM Int
getCountSTM (Counter tm) = countManaged tm
