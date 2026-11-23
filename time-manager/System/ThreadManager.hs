{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A thread manager.
--   The manager has responsibility to kill managed threads.
--
-- Because this is based on the accompanying "System.TimeManager" module,
-- the same caveats apply:
--
--   * Only works for GHC.
--   * Only works with a threaded runtime.
--   * Users of older versions should check the current semantics.
--   * Using 32-bit systems means the max timeout is @'maxBound' :: Int@
--     (2147483647) microseconds, which is less than 36 minutes.
--   * Using the same 'Handle' in different threads might cause issues in some
--     edge cases. (i.e. using cancel/pause in one thread, and resume in another)
module System.ThreadManager (
    ThreadManager,
    newThreadManager,
    withThreadManager,
    stopAfter,
    stopAfterWithResult,
    stopAfterWithGrace,
    waitStoppingSTM,
    KilledByThreadManager (..),

    -- * Fork
    Reservation,
    reserveManaged,
    takeReservation,
    forkManaged,
    forkManagedFinally,
    forkManagedUnmask,
    forkManagedUnmaskFinally,
    forkManagedBy,
    forkManagedByFinally,
    forkManagedTimeout,
    forkManagedTimeoutFinally,

    -- * Results
    Managed,
    forkManagedWithResult,
    StoppedByThreadManager (..),
    ManagedThreadKilled (..),
    managedThreadId,
    waitManagedSTM,
    pollManagedSTM,
    waitManaged,
    waitEitherManaged,
    waitEitherManagedSTM,

    -- * Synchronization
    waitUntilAllGone,
    waitUntilAllGoneFor,
    isAllGone,
    countManaged,
) where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (
    AssertionFailed (..),
    Exception (..),
    SomeException (..),
 )
import qualified Control.Exception as E
import Control.Monad (unless, void, when, (<=<))
import Data.Foldable (forM_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Conc.Sync (labelThread)
#if __GLASGOW_HASKELL__ >= 908
import GHC.Conc.Sync (fromThreadId)
#endif
import System.Mem.Weak (Weak, deRefWeak)
import qualified System.TimeManager as T

----------------------------------------------------------------

-- | Manager to own threads.
data ThreadManager = ThreadManager
    { managedThreads :: TVar ManagedThreads
    , reservedPlaces :: TVar Int
    -- ^ Places kept by 'reserveManaged' for threads not started yet.
    , managerStopped :: TVar Bool
    -- ^ Stopped by 'stopAfter'.  A thread which registers after that
    --   is refused, and does not run its body: it was forked before the
    --   stop and would otherwise run on with nobody owning it.
    }

type Key = Word64
type ManagedThreads = Map Key ManagedThread

----------------------------------------------------------------

-- The 'IORef' keeps the kills of the timeout of 'forkManagedTimeout'
-- and of 'stopAfter' in order with each other and with the thread's end:
-- once the thread is finishing, no kill is decided on, and one decided on
-- before is waited for there and caught, rather than cutting the cleanup
-- short or landing after the thread has left.
data ManagedThread = ManagedThread (Weak ThreadId) (IORef Kill)

data Kill
    = Alive
    | -- | Thrown by a thread of its own, filled once the exception is
      --   taken.  A thread is killed once: one which takes the exception
      --   and carries on breaks the contract, and is not chased.
      Killing (MVar ())
    | -- | Whether it was killed.
      Finishing Bool

----------------------------------------------------------------

-- | Create a thread manager.
--
-- A manager owns threads; it does not time them out.  A caller which wants
-- both passes its own 'T.Manager' to 'forkManagedTimeout'.
newThreadManager :: IO ThreadManager
newThreadManager = do
    managedThreads <- newTVarIO Map.empty
    reservedPlaces <- newTVarIO 0
    managerStopped <- newTVarIO False
    return ThreadManager{..}

----------------------------------------------------------------

-- | A place kept for a thread that is not running yet.
--
--   Registering a thread needs its 'ThreadId', and it is the thread
--   itself which provides it, by asking for its own.  A caller which
--   forks through a function that hands no identity back cannot do it on
--   the thread's behalf, so until the thread arrives and registers
--   itself, what is counted is the place kept for it.
newtype Reservation = Reservation (IORef Bool)

-- | Keeping a place for a thread about to be forked.  It counts towards
--   'countManaged' from here, so that a manager stopped in between does
--   not see a connection that has been accepted as one that is gone.
--   A place that is never taken is never given back, so a caller which
--   reserves and then fails to fork leaves the manager counting it.
reserveManaged :: ThreadManager -> IO Reservation
reserveManaged ThreadManager{..} = E.mask_ $ do
    atomically $ modifyTVar' reservedPlaces (+ 1)
    Reservation <$> newIORef False

-- | Running the action of a thread which was reserved for, taking the
--   place kept for it: the thread records itself, so that 'stopAfter' can
--   reach it, and gives the reservation back.
--
--   The reservation is given back only once the thread is recorded, so
--   that it is counted throughout.
--   A thread which gets here after 'stopAfter' has stopped the manager is
--   refused rather than registered: the action is not run, and this
--   returns.
--
--   The reservation must be one kept by this manager.  Taking one kept by
--   another lowers this manager's count instead of that one's: the other
--   manager never reaches zero, and once this count goes negative,
--   'countManaged', 'isAllGone' and 'waitUntilAllGone' throw 'AssertionFailed'.
takeReservation :: ThreadManager -> Reservation -> IO () -> IO ()
takeReservation mgr rsv io = taking mgr rsv (return ()) $ \_ -> io

-- | Giving a place back, at most once however often this is called: the
--   thread which takes it gives it back, and so does a caller whose fork
--   throws, and only one of them can have it.
release :: ThreadManager -> Reservation -> IO ()
release ThreadManager{..} (Reservation ref) = E.mask_ $ do
    taken <- atomicModifyIORef' ref (\b -> (True, b))
    unless taken $ atomically $ modifyTVar' reservedPlaces (subtract 1)

----------------------------------------------------------------

-- | The exception a manager kills its threads with.  A thread ends quietly
--   on one its own manager threw; one thrown otherwise ends it as any
--   other exception does.
data KilledByThreadManager = KilledByThreadManager (Maybe SomeException)
    deriving (Show)

instance Exception KilledByThreadManager where
    toException = E.asyncExceptionToException
    fromException = E.asyncExceptionFromException

-- | Running an action with a manager of its own, which is stopped once the
--   action is done, however it ends: the threads left are killed, as the
--   last resort, and waited for for at most this many microseconds.  A
--   thread which does not end by then is left, still counted.
--
--   This kills at once.  Threads which should end by themselves first are
--   told by the caller's own means, or stopped with 'stopAfterWithGrace'
--   inside: a thread that one has killed is not killed again here.
--
--   An exception arriving meanwhile ends the wait, and is thrown from
--   here in place of the action's result.
--
-- @since 0.5.3
withThreadManager :: Int -> (ThreadManager -> IO a) -> IO a
withThreadManager cap action = do
    mgr <- newThreadManager
    stopAfterWithResult mgr (action mgr) $ \r -> do
        void $ waitUntilAllGoneFor mgr cap
        either E.throwIO return r

-- | Stopping the manager.
--
-- @
-- stopAfter threadManager action cleanup
-- @
--
-- The action is run in the scope of an exception handler that catches all
-- exceptions (including asynchronous ones); this allows the cleanup handler
-- to cleanup in all circumstances. If an exception is caught, it is rethrown
-- after the cleanup is complete.
--
-- This tells the managed threads to stop; it does not wait for them to be
-- gone, and neither does the cleanup handler.  They are the manager\'s
-- until they end, so 'countManaged' goes on counting them -- which is what
-- a caller waits on with 'waitUntilAllGone' before it frees what those
-- threads were using.
--
-- A thread whose place is still being kept by 'reserveManaged' is not told
-- but refused when it arrives, and never runs.
--
-- A thread which swallows the exception and never ends is therefore
-- counted for ever, and 'waitUntilAllGone' on such a manager never
-- returns.  Nothing here can make it end; a caller which cannot rule that
-- out should put its own timeout around the wait.
stopAfter :: ThreadManager -> IO a -> (Maybe SomeException -> IO ()) -> IO a
stopAfter mgr action cleanup =
    stopAfterWithResult mgr action $ \mResult ->
        case mResult of
            Left err -> do
                cleanup (Just err)
                E.throwIO err
            Right result -> do
                cleanup Nothing
                return result

-- | Generalization of 'stopAfter' where the cleanup handler is allowed to construct the final result
--
-- Unlike in 'stopAfter', if an exception is thrown, it is not re-thrown after the cleanup
-- handler completes; the cleanup handler itself can decide to rethrow it or compute a result.
--
-- @since 0.3.2
stopAfterWithResult
    :: ThreadManager -> IO a -> (Either SomeException a -> IO b) -> IO b
stopAfterWithResult mgr@ThreadManager{..} action cleanup = do
    E.mask $ \unmask -> do
        ma <- E.try $ unmask action
        -- The threads are left in the map rather than taken out of it:
        -- 'throwTo' waits for the exception to be taken, not for the
        -- thread to be gone, and a manager which reports that it owns
        -- nothing while its threads are still unwinding is no use to a
        -- caller which frees what they were using.  They should take
        -- themselves out as they end, and 'managerStopped' is what keeps
        -- new ones from arriving.
        atomically $ writeTVar managerStopped True
        killAll mgr ma
        cleanup ma

-- | Killing every thread the manager has, telling them how the action
--   ended.  This does not wait for the exceptions to be taken: a thread
--   in a foreign call takes one only once the call returns, and the
--   others are not to wait for it.  A thread killed before, by its timeout
--   or an earlier stop, is unwinding and is not killed again.
killAll :: ThreadManager -> Either SomeException a -> IO ()
killAll ThreadManager{..} ma = do
    ths <- Map.elems <$> readTVarIO managedThreads
    let ex = KilledByThreadManager $ either Just (const Nothing) ma
    forM_ ths $ \(ManagedThread wtid ref) -> killWith wtid ref ex

-- | Like 'stopAfter', but the threads are told rather than killed.
--
-- Once the action is done, the manager is stopped as by 'stopAfter', which
-- 'waitStoppingSTM' tells the threads.  They are given this many
-- microseconds to end by themselves, and only the ones still there after
-- that are killed, as the last resort.  The cleanup runs then.
--
-- This is for threads of the caller's own, which wait for
-- 'waitStoppingSTM' alongside what they wait for.  Threads running code
-- which knows nothing of it are better stopped with 'stopAfter'.
--
-- An exception arriving in the grace ends it: the threads still there are
-- killed then, the cleanup runs, still given the action's outcome, and that
-- exception is thrown from here in place of the action's result.
--
-- @since 0.5.3
stopAfterWithGrace
    :: ThreadManager -> Int -> IO a -> (Maybe SomeException -> IO ()) -> IO a
stopAfterWithGrace mgr@ThreadManager{..} grace action cleanup =
    E.mask $ \unmask -> do
        ma <- E.try $ unmask action
        atomically $ writeTVar managerStopped True
        -- An exception landing in the grace ends it, and is thrown once
        -- the last resort and the cleanup have run.
        waited <- E.try (waitUntilAllGoneFor mgr grace) :: IO (Either SomeException Bool)
        case waited of
            Right True -> return ()
            _ -> killAll mgr ma
        cleanup $ either Just (const Nothing) ma
        either E.throwIO (const $ either E.throwIO return ma) waited

-- | Succeeding once the manager has been stopped, by 'stopAfter' or
--   'stopAfterWithGrace'.  A managed thread waits for this alongside what
--   it waits for, and ends by itself when it succeeds.
--
-- @since 0.5.3
waitStoppingSTM :: ThreadManager -> STM ()
waitStoppingSTM ThreadManager{..} = readTVar managerStopped >>= check

----------------------------------------------------------------

-- | Fork a managed thread.
--
-- A place is kept for the thread before it is forked, so that it counts
-- towards 'countManaged' from this call rather than from the moment the
-- run-time system gives the thread its first step.  The thread adds
-- itself to the manager's queue as the first thing it does, takes the
-- place over, and is removed again when it terminates (normally or
-- abnormally).
forkManaged
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> IO ()
    -- ^ Action
    -> IO ()
forkManaged mgr label io = forkManagedFinally mgr label io $ return ()

-- | Like 'forkManaged', but with a cleanup function, run regardless of how
--   the action ends.  An exception from the action is not caught: it ends
--   the thread as it would under 'forkManaged'.
forkManagedFinally
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> IO ()
    -- ^ Action
    -> IO ()
    -- ^ Cleanup function
    -> IO ()
forkManagedFinally mgr label io final =
    forkManagedUnmaskFinally mgr label (\restore -> restore io) final

-- | Like 'forkManaged', but run the action with exceptions masked
--   until the provided `unmask` is used. (i.e. @(forall x. IO x -> IO x)@)
forkManagedUnmask
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Action with unmask argument
    -> IO ()
forkManagedUnmask mgr label io = forkManagedUnmaskFinally mgr label io $ return ()

-- | Like 'forkManagedUnmask', but with a cleanup function, run regardless of
--   how the action ends.  An exception from the action is not caught: it
--   ends the thread as it would under 'forkManagedUnmask'.
forkManagedUnmaskFinally
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Action with unmask argument
    -> IO ()
    -- ^ Cleanup: will always run, regardless of how the thread ends
    -> IO ()
forkManagedUnmaskFinally mgr label io final =
    E.mask_ $
        forkManagedByFinally mgr (\body -> void $ forkIOWithUnmask body) label io final

-- | Fork a managed thread with a fork of the caller's own, for a caller
--   which does not fork with 'forkIO'.  (The @warp@ library, for example,
--   forks the thread serving a connection through a function of its
--   settings, so that the user can replace it.)
--
--   A place is kept for the thread before the fork, so that it counts
--   towards 'countManaged' from here rather than from the moment the
--   thread starts, and the place is given back whether the fork makes the
--   thread or throws.  A thread can belong to a manager only once at a
--   time: do not pass a fork which runs the action in the calling thread
--   from a thread this manager already owns.  A caller which forks by
--   itself can do the same with 'reserveManaged' and 'takeReservation',
--   which this is made of.
forkManagedBy
    :: ThreadManager
    -> (((forall x. IO x -> IO x) -> IO ()) -> IO a)
    -- ^ The fork to make the thread with
    -> String
    -- ^ Thread name
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Action with unmask argument
    -> IO a
forkManagedBy mgr fork label io = forkManagedByFinally mgr fork label io $ return ()

-- | Like 'forkManagedBy', but with a cleanup function.
forkManagedByFinally
    :: ThreadManager
    -> (((forall x. IO x -> IO x) -> IO ()) -> IO a)
    -- ^ The fork to make the thread with
    -> String
    -- ^ Thread name
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Action with unmask argument
    -> IO ()
    -- ^ Cleanup: will always run, regardless of how the thread ends
    -> IO a
forkManagedByFinally mgr fork label io final =
    E.bracketOnError (reserveManaged mgr) (release mgr) $ \rsv ->
        fork $ \unmask -> managed mgr rsv label final $ \_ -> io unmask

-- | Fork a managed thread with a handle created by a timeout manager.
--
--   The time manager is the caller's, since this manager has none: owning a
--   thread and timing it out are separate things, and only this pair of
--   functions needs both.  What needs them together is the state which
--   orders the timeout's kill and 'stopAfter''s with each other and with
--   the thread's end.
forkManagedTimeout
    :: ThreadManager
    -> T.Manager
    -- ^ Time manager
    -> String
    -- ^ Thread name
    -> (T.Handle -> IO ())
    -- ^ Action with timeout handle
    -> IO ()
forkManagedTimeout mgr timmgr label io =
    forkManagedTimeoutFinally mgr timmgr label io $ return ()

-- | Like 'forkManagedTimeout', but with a cleanup function, run regardless of
--   how the action ends.  An exception from the action is not caught: it
--   ends the thread as it would under 'forkManagedTimeout'.
forkManagedTimeoutFinally
    :: ThreadManager
    -> T.Manager
    -- ^ Time manager
    -> String
    -- ^ Thread name
    -> (T.Handle -> IO ())
    -- ^ Action with timeout handle
    -> IO ()
    -- ^ Cleanup: will always run, regardless of how the thread ends
    -> IO ()
forkManagedTimeoutFinally mgr timmgr label io final =
    E.bracketOnError (reserveManaged mgr) (release mgr) $ \rsv ->
        void $ forkIO $
            managed mgr rsv label final $ \(_n, ManagedThread wtid ref) ->
                T.withHandle timmgr (killWith wtid ref ex) io
  where
    ex = KilledByThreadManager Nothing

----------------------------------------------------------------

-- | A managed thread whose end can be waited for, as an @Async@ of the
--   async package can: its result, or the exception it ended with.
--
-- @since 0.5.3
data Managed a = Managed ThreadId (TMVar (Either SomeException a))

-- | Fork a managed thread whose end is kept for 'waitManagedSTM'.
--
--   Nothing it ends with is thrown on, so nothing is printed: what to do
--   with it is the waiter's.  The waiter is handed synchronous exceptions
--   only, since it is not the one killed: 'StoppedByThreadManager' once the
--   manager killed the thread or did not run it, so a waiter never waits
--   for ever, and 'ManagedThreadKilled' for any other asynchronous one.
--
-- @since 0.5.3
forkManagedWithResult :: ThreadManager -> String -> IO a -> IO (Managed a)
forkManagedWithResult mgr label io = do
    var <- newEmptyTMVarIO
    let put = atomically . void . tryPutTMVar var
        stopped = put $ Left $ E.toException StoppedByThreadManager
    -- Masked but for the action, so that a kill cannot land between the
    -- action's end and its being kept.  The action runs in the caller's
    -- masking state, as async's does.
    tid <-
        E.mask $ \restore ->
            E.bracketOnError (reserveManaged mgr) (release mgr) $ \rsv ->
                forkIO $ managed mgr rsv label stopped $ \(_, ManagedThread _ ref) ->
                    E.try (restore io) >>= either (put . Left <=< forWaiter ref) (put . Right)
    return $ Managed tid var
  where
    -- The kill of its manager's, or someone else's asynchronous exception,
    -- as what its waiter is handed.
    forWaiter ref e = case E.fromException e of
        Just (E.SomeAsyncException _) -> do
            k <- readIORef ref
            return $ case (E.fromException e, k) of
                (Just (KilledByThreadManager _), Killing _) -> E.toException StoppedByThreadManager
                _ -> E.toException $ ManagedThreadKilled e
        Nothing -> return e

-- | What a thread of 'forkManagedWithResult' ended with for its waiter when
--   its manager killed it or did not run it.  Synchronous: the waiter was
--   not killed.
--
-- @since 0.5.3
data StoppedByThreadManager = StoppedByThreadManager
    deriving (Show)

instance Exception StoppedByThreadManager

-- | What a thread of 'forkManagedWithResult' ended with for its waiter when
--   an asynchronous exception of someone else's ended it.  Synchronous: the
--   waiter was not killed.
--
-- @since 0.5.3
newtype ManagedThreadKilled = ManagedThreadKilled SomeException
    deriving (Show)

instance Exception ManagedThreadKilled

-- | The thread of 'forkManagedWithResult', as for a last resort of the
--   caller's own.
--
-- @since 0.5.3
managedThreadId :: Managed a -> ThreadId
managedThreadId (Managed tid _) = tid

-- | Succeeding with how a thread of 'forkManagedWithResult' ended, once
--   it has.
--
-- @since 0.5.3
waitManagedSTM :: Managed a -> STM (Either SomeException a)
waitManagedSTM (Managed _ var) = readTMVar var

-- | How a thread of 'forkManagedWithResult' ended, if it has.
--
-- @since 0.5.3
pollManagedSTM :: Managed a -> STM (Maybe (Either SomeException a))
pollManagedSTM (Managed _ var) = tryReadTMVar var

-- | What a thread of 'forkManagedWithResult' returned, or its exception
--   thrown on, as @wait@ of the async package.
--
-- @since 0.5.3
waitManaged :: Managed a -> IO a
waitManaged m = waiting (waitManagedSTM m) >>= either E.throwIO return

-- | 'waitEitherManagedSTM' in 'IO', waiting as 'waitManaged' does.
--
-- @since 0.5.3
waitEitherManaged :: Managed a -> Managed b -> IO (Either a b)
waitEitherManaged a b = waiting $ waitEitherManagedSTM a b

-- | Waiting for threads of 'forkManagedWithResult', again if told that the
--   wait is blocked indefinitely.  Once nothing that could wake the threads
--   is reachable, the run-time system throws that to them and to whoever
--   waits for them at once, but they then end and say so.  Once, as async
--   does: again and again is a loop of collections for the run-time system.
waiting :: STM a -> IO a
waiting stm = atomically stm `E.catch` \E.BlockedIndefinitelyOnSTM -> atomically stm

-- | The first of two threads of 'forkManagedWithResult' to end, its
--   exception thrown on, as @waitEitherSTM@ of the async package.
--
-- @since 0.5.3
waitEitherManagedSTM :: Managed a -> Managed b -> STM (Either a b)
waitEitherManagedSTM a b =
    (Left <$> (waitManagedSTM a >>= either throwSTM return))
        `orElse` (Right <$> (waitManagedSTM b >>= either throwSTM return))

----------------------------------------------------------------

-- | The body of a managed thread, under its own label, taking the place
--   kept for it.
managed
    :: ThreadManager
    -> Reservation
    -> String
    -> IO ()
    -> ((Key, ManagedThread) -> IO ())
    -> IO ()
managed mgr rsv label final body = do
    labelMe label
    taking mgr rsv final body

-- | Running the body of a thread which had a place kept for it.  The
--   place is given back only once the thread is recorded, so that it is
--   counted throughout.
taking
    :: ThreadManager
    -> Reservation
    -> IO ()
    -> ((Key, ManagedThread) -> IO ())
    -> IO ()
taking mgr rsv final body =
    recorded mgr (give >> final) $ \key -> give >> body key
  where
    give = release mgr rsv

-- | Running the body of a managed thread with the thread recorded.
--
--   The cleanup runs regardless of how the body ends, and while the thread is still
--   recorded, so that a manager which has none left has none whose cleanup
--   is still running either.
--
--   A thread forked before the manager was stopped and scheduled after it
--   is not recorded and does not run its body: there is nobody left to
--   kill it, and running it is what the stop was for.
recorded
    :: ThreadManager
    -> IO ()
    -> ((Key, ManagedThread) -> IO ())
    -> IO ()
recorded mgr final body = E.mask $ \restore -> do
    -- The handlers are in place before anything is unmasked: a kill
    -- decided on as soon as the thread is recorded lands inside them.
    mkey <- setup mgr
    case mkey of
        Nothing -> final
        Just key@(_, ManagedThread _ ref) ->
            ( (restore (body key) `E.finally` (finishing ref `E.finally` final))
                `E.catch` \e@(KilledByThreadManager _) -> do
                    -- Only a kill of this manager's ends the thread quietly.
                    killed <- readIORef ref
                    case killed of
                        Finishing True -> return ()
                        _ -> E.throwIO e
            )
                `E.finally` clear mgr key

-- | No kill is decided on from here, and one decided on before is waited
--   for, taken and caught here.  'readMVar' is interruptible, so it
--   arrives although this runs masked; another exception meanwhile is
--   held until it has, and thrown then.  Uninterruptibly masked, the kill
--   could never arrive, and it is not waited for.
finishing :: IORef Kill -> IO ()
finishing ref = do
    old <- atomicModifyIORef' ref $ \k -> case k of
        Alive -> (Finishing False, k)
        Killing _ -> (Finishing True, k)
        Finishing _ -> (k, k)
    st <- E.getMaskingState
    case old of
        Killing done | st /= E.MaskedUninterruptible -> await done Nothing
        _ -> return ()
  where
    await done other = do
        r <- E.try $ readMVar done
        case r of
            Right () -> mapM_ E.throwIO other
            Left e
                | Just (KilledByThreadManager _) <- E.fromException e -> await done other
                | otherwise -> await done (other <|> Just (e :: SomeException))

-- | Recording this thread, or 'Nothing' once the manager has stopped.
setup
    :: ThreadManager
    -> IO (Maybe (Key, ManagedThread))
setup ThreadManager{..} = do
    (wtid, n) <- myWeakThreadId
    ref <- newIORef Alive
    let ent = ManagedThread wtid ref
    -- asking to throw KilledByThreadManager to me
    atomically $ do
        stopped <- readTVar managerStopped
        if stopped
            then return Nothing
            else do
                modifyTVar' managedThreads $ Map.insert n ent
                return $ Just (n, ent)

-- | Killing a thread, for its timeout or a stop, if it is alive: one
--   killed already, or finishing, is left be.
killWith :: Weak ThreadId -> IORef Kill -> KilledByThreadManager -> IO ()
killWith wtid ref e = do
    k <- readIORef ref
    case k of
        Alive -> do
            won <- killAlive wtid ref e
            -- Lost to the other kill or to the thread's end: looked at again.
            unless won $ killWith wtid ref e
        _ -> return ()

-- | Killing a thread which is still alive, from a thread of its own:
--   'throwTo' blocks until the target takes the exception, and the killer
--   is not to wait for it.  'False' if it was not alive any more.
killAlive :: Weak ThreadId -> IORef Kill -> KilledByThreadManager -> IO Bool
killAlive wtid ref e = E.mask_ $ do
    done <- newEmptyMVar
    won <- atomicModifyIORef' ref $ \k -> case k of
        Alive -> (Killing done, True)
        _ -> (k, False)
    when won $
        void $
            forkIO $
                (deRefWeak wtid >>= mapM_ (`E.throwTo` e))
                    `E.finally` putMVar done ()
    return won

clear
    :: ThreadManager
    -> (Key, ManagedThread)
    -> IO ()
clear ThreadManager{..} (n, _) =
    atomically $ modifyTVar' managedThreads $ Map.delete n

-- | Wait until all managed threads are finished.
--
--   After 'stopAfter' this waits for the threads it stopped to end, not
--   merely for them to have been told.  See there for the thread that
--   never ends.
--
--   A place kept by 'reserveManaged' and never taken is never given back,
--   so this never returns for a caller which reserves and then fails to
--   fork.
waitUntilAllGone :: ThreadManager -> IO ()
waitUntilAllGone tm =
    atomically $
        isAllGone tm >>= check

-- | 'waitUntilAllGone' for at most this many microseconds, answering
--   whether they are all gone.  The time is waited out in STM, not with a
--   timeout's asynchronous exception; the wait is interruptible all the
--   same.
--
-- @since 0.5.3
waitUntilAllGoneFor :: ThreadManager -> Int -> IO Bool
waitUntilAllGoneFor tm us = do
    late <- registerDelay us
    atomically $
        (True <$ (isAllGone tm >>= check)) `orElse` (False <$ (readTVar late >>= check))

-- | STM action that checks if all managed threads are finished.
isAllGone :: ThreadManager -> STM Bool
isAllGone tm = (== 0) <$> countManaged tm

-- | STM action for how many threads are managed, counting the places
--   kept by 'reserveManaged' for threads which have not started, and the
--   threads 'stopAfter' has stopped which have not ended yet.
--
--   Throws 'AssertionFailed' if the count of places has gone negative,
--   which happens when a reservation is taken by a manager other than the
--   one that kept it.  A mistake of that kind which does not drive the
--   count negative goes unnoticed.
countManaged :: ThreadManager -> STM Int
countManaged ThreadManager{..} = do
    places <- readTVar reservedPlaces
    when (places < 0) $
        throwSTM $
            AssertionFailed $
                "System.ThreadManager: "
                    ++ show places
                    ++ " reserved places; a reservation was taken by a"
                    ++ " manager other than its own"
    (+ places) . Map.size <$> readTVar managedThreads

----------------------------------------------------------------

myWeakThreadId :: IO (Weak ThreadId, Key)
myWeakThreadId = do
    tid <- myThreadId
    wtid <- mkWeakThreadId tid
    let n = fromThreadId tid
    return (wtid, n)

labelMe :: String -> IO ()
labelMe l = do
    tid <- myThreadId
    labelThread tid l

#if __GLASGOW_HASKELL__ < 908
fromThreadId :: ThreadId -> Word64
fromThreadId tid = read (drop 9 $ show tid)
#endif
