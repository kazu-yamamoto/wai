{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

-- | A timer for Windows which does not depend on the I\/O manager.
--
--   @GHC.Event.Windows@ is WinIO's manager, so a timeout registered with
--   it is never run in a program not using WinIO (the default), which has
--   no I\/O manager.  This does the same job with one thread per timeout,
--   which works either way.  With @-threaded@ it waits in STM, and a
--   cancel tells it; without, 'registerDelay' is missing, so it sleeps
--   and a cancel kills it.
module System.TimeManager.Windows (
    Manager,
    TimeoutKey,
    getSystemManager,
    registerTimeout,
    unregisterTimeout,
    updateTimeout,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, rtsSupportsBoundThreads, threadDelay)
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

-- | There is nothing to keep: each timeout is its own thread.
data Manager = Manager

getSystemManager :: IO Manager
getSystemManager = pure Manager

-- | A registered timeout.  The deadline is read by the thread rather
--   than captured, so that 'updateTimeout' does not have to replace it.
data TimeoutKey = TimeoutKey (IORef Word64) (IORef State) (TVar Bool) ThreadId

instance Eq TimeoutKey where
    TimeoutKey _ _ _ t1 == TimeoutKey _ _ _ t2 = t1 == t2

-- | Whether the action has started or the timeout has been cancelled,
--   whichever came first: a started action is not cut short, as with
--   @GHC.Event@.
data State = Waiting | Running | Cancelled

-- | Run the action once the given number of microseconds has passed.
registerTimeout :: Manager -> Int -> IO () -> IO TimeoutKey
registerTimeout _ us action = do
    ref <- newIORef =<< deadlineIn us
    st <- newIORef Waiting
    cancelled <- newTVarIO False
    -- Unmasked, whatever the caller's masking: an uninterruptible one
    -- would keep 'unregisterTimeout' waiting on this thread for good.
    tid <- forkIOWithUnmask $ \unmask -> unmask (wait ref st cancelled)
    pure $ TimeoutKey ref st cancelled tid
  where
    wait ref st cancelled = do
        deadline <- readIORef ref
        now <- getMonotonicTimeNSec
        if now >= deadline
            then do
                run <- atomicModifyIORef' st $ \case
                    Waiting -> (Running, True)
                    s -> (s, False)
                when run action
            else do
                -- Round up, so that a deadline under a microsecond away
                -- is still waited for rather than busy looped on.
                let us' = fromIntegral ((deadline - now + 999) `div` 1_000)
                stop <- sleep us' cancelled
                unless stop $ wait ref st cancelled
    -- Whether it was cancelled.
    sleep us' cancelled
        | rtsSupportsBoundThreads = do
            late <- registerDelay us'
            atomically $
                (False <$ (readTVar late >>= check))
                    `orElse` (True <$ (readTVar cancelled >>= check))
        | otherwise = False <$ threadDelay us'

-- | Move the deadline the given number of microseconds into the future.
updateTimeout :: Manager -> TimeoutKey -> Int -> IO ()
updateTimeout _ (TimeoutKey ref _ _ _) us = writeIORef ref =<< deadlineIn us

-- | Cancel the timeout.  An action which has started is left to finish.
--   The waiting thread is told, or without @-threaded@ killed: it is
--   always unmasked.
unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout _ (TimeoutKey _ st cancelled tid) = do
    waiting <- atomicModifyIORef' st $ \case
        Waiting -> (Cancelled, True)
        s -> (s, False)
    when waiting $
        if rtsSupportsBoundThreads
            then atomically $ writeTVar cancelled True
            else killThread tid

deadlineIn :: Int -> IO Word64
deadlineIn us = do
    now <- getMonotonicTimeNSec
    -- Clamped, so that a huge timeout does not wrap into the past.
    pure $ now + min (maxBound `div` 2_000) (fromIntegral (max 0 us)) * 1_000
