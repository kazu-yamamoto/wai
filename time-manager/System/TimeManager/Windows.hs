{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

-- | A timer for Windows which does not depend on the I\/O manager.
--
--   @GHC.Event.Windows@ is WinIO's manager, so a timeout registered with
--   it is never run in a program not using WinIO (the default), which has
--   no I\/O manager.  This does the same job with one thread and
--   'threadDelay' per timeout, which works either way.
module System.TimeManager.Windows (
    Manager,
    TimeoutKey,
    getSystemManager,
    registerTimeout,
    unregisterTimeout,
    updateTimeout,
) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, threadDelay)
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

-- | There is nothing to keep: each timeout is its own thread.
data Manager = Manager

getSystemManager :: IO Manager
getSystemManager = pure Manager

-- | A registered timeout.  The deadline is read by the thread rather
--   than captured, so that 'updateTimeout' does not have to replace it.
data TimeoutKey = TimeoutKey (IORef Word64) (IORef State) ThreadId

instance Eq TimeoutKey where
    TimeoutKey _ _ t1 == TimeoutKey _ _ t2 = t1 == t2

-- | Whether the action has started or the timeout has been cancelled,
--   whichever came first: a started action is not cut short, as with
--   @GHC.Event@.
data State = Waiting | Running | Cancelled

-- | Run the action once the given number of microseconds has passed.
registerTimeout :: Manager -> Int -> IO () -> IO TimeoutKey
registerTimeout _ us action = do
    ref <- newIORef =<< deadlineIn us
    st <- newIORef Waiting
    -- Unmasked, whatever the caller's masking: an uninterruptible one
    -- would keep 'unregisterTimeout' waiting on this thread for good.
    tid <- forkIOWithUnmask $ \unmask -> unmask (wait ref st)
    pure $ TimeoutKey ref st tid
  where
    wait ref st = do
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
                threadDelay $ fromIntegral ((deadline - now + 999) `div` 1_000)
                wait ref st

-- | Move the deadline the given number of microseconds into the future.
updateTimeout :: Manager -> TimeoutKey -> Int -> IO ()
updateTimeout _ (TimeoutKey ref _ _) us = writeIORef ref =<< deadlineIn us

-- | Cancel the timeout.  An action which has started is left to finish;
--   the thread, always unmasked, is killed only while it still waits.
unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout _ (TimeoutKey _ st tid) = do
    waiting <- atomicModifyIORef' st $ \case
        Waiting -> (Cancelled, True)
        s -> (s, False)
    when waiting $ killThread tid

deadlineIn :: Int -> IO Word64
deadlineIn us = do
    now <- getMonotonicTimeNSec
    -- Clamped, so that a huge timeout does not wrap into the past.
    pure $ now + min (maxBound `div` 2_000) (fromIntegral (max 0 us)) * 1_000
