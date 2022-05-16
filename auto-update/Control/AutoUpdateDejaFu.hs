{-# LANGUAGE ScopedTypeVariables #-}

-- | A common problem is the desire to have an action run at a scheduled
-- interval, but only if it is needed. For example, instead of having
-- every web request result in a new @getCurrentTime@ call, we'd like to
-- have a single worker thread run every second, updating an @IORef@.
-- However, if the request frequency is less than once per second, this is
-- a pessimization, and worse, kills idle GC.
--
-- This library allows you to define actions which will either be
-- performed by a dedicated thread or, in times of low volume, will be
-- executed by the calling thread.
module Main where

import           Control.Exception        (SomeException)
import           Control.Monad
import           Control.Monad.Conc.Class

import Test.DejaFu

main :: IO ()
main = void $ autocheck deadlocks

-- This exhibits a deadlock with no preemptions.
deadlocks :: MonadConc m => m ()
deadlocks = join (mkAutoUpdate defaultUpdateSettings)

-- This exhibits nondeterminism with three preemptions.  However, as
-- the program explicitly yields, the bounds don't need changing.
nondeterministic :: forall m. MonadConc m => m Int
nondeterministic = do
  var <- newIORef 0
  let settings = (defaultUpdateSettings :: UpdateSettings m ())
        { updateAction = atomicModifyIORef var (\x -> (x+1, x)) }
  auto <- mkAutoUpdate settings
  void auto
  auto

-------------------------------------------------------------------------------

-- | Default value for creating an @UpdateSettings@.
--
-- Since 0.1.0
defaultUpdateSettings :: MonadConc m => UpdateSettings m ()
defaultUpdateSettings = UpdateSettings
    { updateFreq = 1000000
    , updateSpawnThreshold = 3
    , updateAction = return ()
    }

-- | Settings to control how values are updated.
--
-- This should be constructed using @defaultUpdateSettings@ and record
-- update syntax, e.g.:
--
-- @
-- let set = defaultUpdateSettings { updateAction = getCurrentTime }
-- @
--
-- Since 0.1.0
data UpdateSettings m a = UpdateSettings
    { updateFreq           :: Int
    -- ^ Microseconds between update calls. Same considerations as
    -- @threadDelay@ apply.
    --
    -- Default: 1 second (1000000)
    --
    -- Since 0.1.0
    , updateSpawnThreshold :: Int
    -- ^ NOTE: This value no longer has any effect, since worker threads are
    -- dedicated instead of spawned on demand.
    --
    -- Previously, this determined: How many times the data must be requested
    -- before we decide to spawn a dedicated thread.
    --
    -- Default: 3
    --
    -- Since 0.1.0
    , updateAction         :: m a
    -- ^ Action to be performed to get the current value.
    --
    -- Default: does nothing.
    --
    -- Since 0.1.0
    }

-- | Generate an action which will either read from an automatically
-- updated value, or run the update action in the current thread.
--
-- Since 0.1.0
mkAutoUpdate :: MonadConc m => UpdateSettings m a -> m (m a)
mkAutoUpdate us = do
    -- The current value, if available.
    currRef <- newIORef Nothing

    -- A baton to tell the worker thread to generate a new value.
    needsRunning <- newEmptyMVar

    -- The last value generated, to allow for blocking semantics when currRef
    -- is Nothing.
    lastValue <- newEmptyMVar

    -- fork the worker thread immediately...
    void $ fork $ forever $ do
        -- but block until a value is actually needed
        takeMVar needsRunning

        -- new value requested, so run the updateAction
        a <- catchSome $ updateAction us

        -- we got a new value, update currRef and lastValue
        writeIORef currRef $ Just a
        void $ tryTakeMVar lastValue
        putMVar lastValue a

        -- delay until we're needed again
        threadDelay $ updateFreq us

        -- delay's over, clear out currRef and lastValue so that demanding the
        -- value again forces us to start work
        writeIORef currRef Nothing
        void $ takeMVar lastValue

    return $ do
        mval <- readIORef currRef
        case mval of
            -- we have a current value, use it
            Just val -> return val
            Nothing -> do
                -- no current value, force the worker thread to run...
                void $ tryPutMVar needsRunning ()

                -- and block for the result from the worker
                readMVar lastValue

-- | Turn a runtime exception into an impure exception, so that all @IO@
-- actions will complete successfully. This simply defers the exception until
-- the value is forced.
catchSome :: MonadConc m => m a -> m a
catchSome act = catch act $
  \e -> throw (e :: SomeException)
