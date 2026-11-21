{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec

#if defined(mingw32_HOST_OS)
import qualified System.TimeManager.Windows as EV
#else
import qualified GHC.Event as EV
#endif

import Control.Concurrent (
    MVar,
    forkIO,
    myThreadId,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
    tryPutMVar,
    tryTakeMVar,
    yield,
 )
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception (AssertionFailed (..), finally, throwIO)
import qualified Control.Exception as E
import Control.Monad (forM_, unless, void, when)
import Data.Maybe (isJust, isNothing)
import System.Mem (performGC)
import Data.IORef as I (
    IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
    writeIORef,
 )
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc.Sync (
    getUncaughtExceptionHandler,
    setUncaughtExceptionHandler,
 )
import System.ThreadManager (
    KilledByThreadManager,
    ThreadManager,
    countManaged,
    forkManaged,
    forkManagedFinally,
    forkManagedTimeout,
    forkManagedTimeoutFinally,
    forkManagedUnmaskFinally,
    concurrentlyManaged_,
    forkManagedWithResult,
    managedThreadId,
    raceManaged_,
    newThreadManager,
    pollManagedSTM,
    reserveManaged,
    stopAfter,
    stopAfterWithGrace,
    takeReservation,
    withThreadManager,
    waitManagedSTM,
    waitStoppingSTM,
    waitUntilAllGone,
    waitUntilAllGoneFor,
 )
import System.TimeManager
import System.TimeManager.Internal
import System.Watchdog (
    WatchdogFor (..),
    newWatchdog,
    setTimeoutProbing,
    timedOutSTM,
    update,
    withWatchdog,
 )
import qualified System.Timeout as Timeout
import Test.HUnit (assertBool)

main :: IO ()
main = hspec $ do
    describe "Watchdog, probing" $ do
        it "keeps a task whose peer answers the probe" $ do
            (r, probes) <- probing True
            r `shouldBe` Nothing
            probes `shouldSatisfy` (>= 2)
        it "times out a task whose peer does not answer, after one probe" $ do
            (r, probes) <- probing False
            r `shouldBe` Just ()
            probes `shouldBe` 1
    describe "ThreadManager, once stopped" $ do
        -- 'setup' refuses to record a thread forked before the stop and
        -- scheduled after it, by throwing.  That must not escape the fork,
        -- and must not take the cleanup with it.
        it "does not let forkManagedTimeout's refusal escape the fork" $ do
            tm <- newThreadManager
            stopAfter tm (return ()) $ \_ -> return ()
            ran <- freshRef
            escaped <- freshRef
            handler0 <- getUncaughtExceptionHandler
            setUncaughtExceptionHandler $ \_ -> I.writeIORef escaped True
            forkManagedTimeout tm defaultManager "probe" $ \_ -> I.writeIORef ran True
            threadDelay 200_000
            setUncaughtExceptionHandler handler0
            ran `refShouldBe` False
            escaped `refShouldBe` False

        itRunsTheCleanupOf "forkManagedFinally" $
            \tm label -> forkManagedFinally tm label (return ())

        itRunsTheCleanupOf "forkManagedTimeoutFinally" $
            \tm label -> forkManagedTimeoutFinally tm defaultManager label (\_ -> return ())

        itRunsTheCleanupOf "forkManagedUnmaskFinally" $
            \tm label -> forkManagedUnmaskFinally tm label (\_ -> return ())

    describe "ThreadManager, the action of a fork with a cleanup" $ do
        itLetsTheExceptionOutOf "forkManagedFinally" forkManagedFinally

        itLetsTheExceptionOutOf "forkManagedTimeoutFinally" $
            \tm label io -> forkManagedTimeoutFinally tm defaultManager label (\_ -> io)

        itLetsTheExceptionOutOf "forkManagedUnmaskFinally" $
            \tm label io -> forkManagedUnmaskFinally tm label (\unmask -> unmask io)

    describe "ThreadManager, threads with a result" $ do
        it "keeps what the thread returned" $ do
            tm <- newThreadManager
            m <- forkManagedWithResult tm "probe" $ return (42 :: Int)
            r <- atomically $ waitManagedSTM m
            either (const Nothing) Just r `shouldBe` Just 42
            waitUntilAllGoneFor tm 1_000_000 `shouldReturn` True

        it "keeps what the thread threw, and does not throw it on" $ do
            tm <- newThreadManager
            m <- forkManagedWithResult tm "probe" $ throwIO (userError "boom") >> return ()
            r <- atomically $ waitManagedSTM m
            either (\e -> show e) (const "") r `shouldBe` show (userError "boom")

        it "is stopped with the scope of withThreadManager" $ do
            m <- withThreadManager $ \tm -> forkManagedWithResult tm "probe" $ threadDelay 10_000_000
            -- Killed and waited for by then.
            r <- atomically $ pollManagedSTM m
            fmap (either (isJust . (E.fromException :: E.SomeException -> Maybe KilledByThreadManager)) (const False)) r
                `shouldBe` Just True

        it "waits for both with concurrentlyManaged_, and throws the first exception" $ do
            done <- newEmptyMVar
            concurrentlyManaged_ "a" (threadDelay 50_000 >> putMVar done ()) "b" (return ())
            tryTakeMVar done `shouldReturn` Just ()
            r <- E.try $ concurrentlyManaged_ "a" (threadDelay 10_000_000) "b" (throwIO $ userError "b")
            either (\e -> show (e :: E.SomeException)) (const "") r `shouldBe` show (userError "b")

        it "waits again when told that the wait is blocked indefinitely" $ do
            -- Nothing can wake the thread "a" waits on, so the run-time
            -- system tells it, and its waiter, at once; "a" then ends.
            res <- newEmptyMVar
            let stuck =
                    (STM.newEmptyTMVarIO >>= atomically . STM.takeTMVar)
                        `E.catch` \E.BlockedIndefinitelyOnSTM -> return ()
            _ <- forkIO $ E.try (concurrentlyManaged_ "a" stuck "b" (return ())) >>= putMVar res
            _ <- forkIO $ forM_ [1 .. 20 :: Int] $ \_ -> threadDelay 50_000 >> performGC
            r <- Timeout.timeout 3_000_000 $ takeMVar res
            fmap (either (\e -> show (e :: E.SomeException)) (const "done")) r `shouldBe` Just "done"

        it "ends with the first with raceManaged_, and stops the other" $ do
            stopped <- newEmptyMVar
            raceManaged_
                "a"
                (threadDelay 10_000_000 `E.onException` putMVar stopped ())
                "b"
                (return ())
            tryTakeMVar stopped `shouldReturn` Just ()

        it "tells the thread it runs on" $ do
            tm <- newThreadManager
            var <- newEmptyMVar
            m <- forkManagedWithResult tm "probe" $ myThreadId >>= putMVar var
            takeMVar var `shouldReturn` managedThreadId m

        it "tells nothing before the thread has ended" $ do
            tm <- newThreadManager
            go <- newEmptyMVar
            m <- forkManagedWithResult tm "probe" $ takeMVar go
            atomically (isNothing <$> pollManagedSTM m) `shouldReturn` True
            putMVar go ()
            r <- atomically $ waitManagedSTM m
            either (const False) (const True) r `shouldBe` True

        it "ends with the kill of the manager stopping" $ do
            tm <- newThreadManager
            m <- forkManagedWithResult tm "probe" $ threadDelay 10_000_000
            threadDelay 50_000
            stopAfter tm (return ()) $ \_ -> return ()
            r <- Timeout.timeout 1_000_000 $ atomically $ waitManagedSTM m
            fmap (either (isJust . (E.fromException :: E.SomeException -> Maybe KilledByThreadManager)) (const False)) r
                `shouldBe` Just True

        it "ends a thread the stopped manager does not run" $ do
            tm <- newThreadManager
            stopAfter tm (return ()) $ \_ -> return ()
            m <- forkManagedWithResult tm "probe" $ return ()
            r <- Timeout.timeout 1_000_000 $ atomically $ waitManagedSTM m
            fmap (either (isJust . (E.fromException :: E.SomeException -> Maybe KilledByThreadManager)) (const False)) r
                `shouldBe` Just True

    describe "ThreadManager, stopped with a grace" $ do
        it "lets a thread waiting for the stop end by itself" $ do
            tm <- newThreadManager
            ended <- newEmptyMVar
            forkManaged tm "probe" $
                (atomically (waitStoppingSTM tm) >> putMVar ended "itself")
                    `E.catch` \(E.SomeException e) -> putMVar ended (show e)
            threadDelay 50_000
            stopAfterWithGrace tm 1_000_000 (return ()) $ \_ -> return ()
            takeMVar ended `shouldReturn` "itself"
            atomically (countManaged tm) `shouldReturn` 0

        it "kills a thread which does not end within the grace" $ do
            tm <- newThreadManager
            forkManaged tm "probe" $ threadDelay 10_000_000
            threadDelay 50_000
            t0 <- getMonotonicTimeNSec
            stopAfterWithGrace tm 200_000 (return ()) $ \_ -> return ()
            t1 <- getMonotonicTimeNSec
            (t1 - t0) `shouldSatisfy` (>= 150_000_000)
            waitUntilAllGoneFor tm 1_000_000 `shouldReturn` True

        it "runs the cleanup and rethrows when the action throws" $ do
            tm <- newThreadManager
            cleaned <- newEmptyMVar
            r <-
                E.try $
                    stopAfterWithGrace tm 100_000 (throwIO $ userError "action") $
                        putMVar cleaned . fmap show
            either (\(E.SomeException _) -> True) (const False) r `shouldBe` True
            takeMVar cleaned `shouldReturn` Just (show $ userError "action")

        it "says when the threads are not gone in time" $ do
            tm <- newThreadManager
            forkManaged tm "probe" $ threadDelay 10_000_000
            waitUntilAllGoneFor tm 100_000 `shouldReturn` False
            stopAfter tm (return ()) $ \_ -> return ()

    describe "ThreadManager" $ do
        it "fails loudly when a reservation is taken by another manager" $ do
            a <- newThreadManager
            b <- newThreadManager
            rsv <- reserveManaged a
            takeReservation b rsv (return ())
            atomically (countManaged b)
                `shouldThrow` (\(AssertionFailed _) -> True)

        it "has none left only once their cleanups are done" $ do
            tm <- newThreadManager
            ref <- freshRef
            forkManagedFinally
                tm
                "probe"
                (threadDelay 100_000)
                (threadDelay 300_000 >> I.writeIORef ref True)
            -- The action is over and the cleanup is still running.
            threadDelay 200_000
            waitUntilAllGone tm
            ref `refShouldBe` True

        it "still owns a thread it killed until that thread is gone" $ do
            tm <- newThreadManager
            ref <- freshRef
            started <- newEmptyMVar
            -- The cleanup here is the thread's own, not one the manager
            -- runs for it, so what is tested is an ordinary managed
            -- thread unwinding.
            forkManaged tm "probe" $
                (putMVar started () >> threadDelay 10_000_000)
                    `finally` (threadDelay 300_000 >> I.writeIORef ref True)
            takeMVar started
            threadDelay 50_000
            -- 'throwTo' waits for the exception to be taken, not for the
            -- thread to be gone.
            stopAfter tm (return ()) $ \_ -> return ()
            threadDelay 50_000
            atomically (countManaged tm) `shouldReturn` 1
            waitUntilAllGone tm
            ref `refShouldBe` True

        -- A thread timed out while it cannot take the kill must not hold up
        -- the system timer manager, which every 'threadDelay' waits on.
        it "does not stop the timers on a masked thread timing out" $ do
            tm <- newThreadManager
            m <- initialize 10_000
            release <- newEmptyMVar
            forkManagedTimeout tm m "masked" $ \_ ->
                E.uninterruptibleMask_ $ takeMVar release
            -- Released without the timer manager, which may be stuck.
            t0 <- getMonotonicTimeNSec
            _ <- forkIO $ do
                let spin = do
                        now <- getMonotonicTimeNSec
                        unless (now - t0 >= 1_000_000_000) $ yield >> spin
                spin
                putMVar release ()
            threadDelay 100_000
            t1 <- getMonotonicTimeNSec
            (t1 - t0) `shouldSatisfy` (< 500_000_000)

    describe "TimeManager" $ do
        it "defaultManager == no manager" $
            defaultManager `shouldSatisfy` isNoManager

        it "initializes negative manager" $ do
            let check = (`shouldBe` defaultManager)
            initialize (-10) >>= check
            withManager (-5) check

        it "empty handle is correct" $
            handleTimeout emptyHandle `shouldBe` 0

        it "empty handle check is consistent" $ do
            assertBool "emptyHandle not empty" $
                isEmptyHandle emptyHandle

        it "gives emptyHandle when registering defaultManager" $ do
            hndl <- register defaultManager $ pure ()
            assertBool "got non-empty handle" $ isEmptyHandle hndl

        it "throws TimeoutThread exception" $
            throwsTimeoutThread $ do
                mngr <- initialize timeoutAmount
                _hndl <- registerKillThread mngr $ pure ()
                waitLong

        it "defaultManager doesn't kill thread" $ do
            _hndl <- registerKillThread defaultManager $ pure ()
            waitShort

        it "withHandle: registers timeout" $
            withHandleTest mgr1 $ \check _ -> do
                waitShort
                check True

        it "withHandle: doesn't register timeout" $
            withHandleTest defaultManager $ \check _ -> do
                waitShort
                check False

        -- We make a ref on the outside, to check that the ref is indeed
        -- set before the timeout kills the action inside.
        it "withHandleKillThread: registers timeout (and kills)" $ do
            ref <- freshRef
            withHandleKillTest (Just ref) mgr1 $ \_ _ ->
                throwsTimeoutThread waitShort
            ref `refShouldBe` True

        it "withHandleKillThread: doesn't register timeout" $
            withHandleKillTest Nothing defaultManager $ \check _ -> do
                waitShort >> check False

        -- The body ends just as the timeout expires, so that the kill is
        -- decided on while the body is leaving.  It must not land after
        -- 'withHandleKillThread' has returned.
        it "withHandleKillThread: TimeoutThread does not escape" $ do
            m <- initialize 2_000
            escaped <- I.newIORef (0 :: Int)
            forM_ [1 .. 1_000 :: Int] $ \i -> do
                r <- E.try $ do
                    withHandleKillThread m (pure ()) $ \_ ->
                        threadDelay $ 1_900 + i `mod` 200
                    threadDelay 1_000
                case r of
                    Left TimeoutThread -> I.atomicModifyIORef' escaped $ \n -> (n + 1, ())
                    Right () -> pure ()
            I.readIORef escaped `shouldReturn` 0

        -- Likewise, the timeout action must not run once
        -- 'withHandleKillThread' has returned.
        it "withHandleKillThread: no timeout action after it returns" $ do
            m <- initialize 2_000
            late <- I.newIORef (0 :: Int)
            forM_ [1 .. 1_000 :: Int] $ \i -> do
                returned <- I.newIORef False
                let onTimeout = do
                        r <- I.readIORef returned
                        when r $ I.atomicModifyIORef' late $ \n -> (n + 1, ())
                withHandleKillThread m onTimeout $ \_ ->
                    threadDelay $ 1_900 + i `mod` 200
                I.writeIORef returned True
                threadDelay 1_000
            I.readIORef late `shouldReturn` 0

        it "cancel/pause works as expected" $ do
            m <- mkTestManager
            let killUnless f = do
                    hndl <- registerKillThread m (pure ())
                    _ <- f hndl
                    waitLong
            throwsTimeoutThread $ killUnless pure
            killUnless cancel
            killUnless pause

        it "tickle works as expected" $ do
            m <- mkTestManager
            withHandleTest m $ \check hndl -> do
                forM_ [(1 :: Int) .. 20] $ \_ -> do
                    waitShort
                    tickle hndl
                check False

        let runAndWaitForTimeout f =
                runIt $ \hndl -> do
                    void $ f hndl
                    waitLong
        it "resume works as expected (nothing)" $ do
            -- Doing nothing kills the thread
            throwsTimeoutThread . runAndWaitForTimeout $ \_ -> pure ()
        it "resume works as expected (pause)" $ do
            -- Pausing stops the kill
            runAndWaitForTimeout $ \hndl -> do
                waitShort >> pause hndl
        it "resume works as expected (pause/resume)" $ do
            -- Resuming kills the thread again
            throwsTimeoutThread . runAndWaitForTimeout $ \hndl -> do
                waitShort >> pause hndl
                waitLong >> resume hndl
        it "resume works as expected (cancel/resume)" $ do
            -- Cancelling is unresumable
            runAndWaitForTimeout $ \hndl -> do
                waitShort >> cancel hndl
                waitLong >> resume hndl
        it "resume works as expected (cancel/pause/resume)" $ do
            -- Cancelling and then pausing is still unresumable
            runAndWaitForTimeout $ \hndl -> do
                waitShort >> cancel hndl
                waitShort >> pause hndl
                waitLong >> resume hndl
            -- Pausing, then cancelling doesn't change anything
            runAndWaitForTimeout $ \hndl -> do
                waitShort >> pause hndl
                waitShort >> cancel hndl
                waitLong >> resume hndl
        it "finished timeout won't resume" $ do
            -- If the timeout action runs, resume shouldn't work
            counter <- I.newIORef (0 :: Int)
            m <- mkTestManager
            let increase = I.atomicModifyIORef' counter $ \i -> (i + 1, ())
            withHandle m increase $ \h -> do
                let checkCount x = do
                        i <- I.readIORef counter
                        i `shouldBe` x
                    timeoutOnlyRanOnce = waitLong >> checkCount 1

                checkCount 0
                -- waiting lets the timeout
                timeoutOnlyRanOnce
                -- resuming should not influence the counter
                resume h
                timeoutOnlyRanOnce
                -- pausing after it runs also doesn't re-arm the timeout
                pause h
                resume h
                timeoutOnlyRanOnce
                -- cancel also doesn't re-arm the timeout
                cancel h
                pause h
                resume h
                timeoutOnlyRanOnce

        it "reschedules an expired callback for the remaining timeout" $ do
            m <- initialize 1_000_000
            fired <- newEmptyMVar
            withHandle m (putMVar fired ()) $ \h -> do
                threadDelay 500_000
                -- Model a renewal after GHC has queued the original callback.
                -- Updating GHC's timer here would remove that stale callback.
                withLock (handleLock h) $ do
                    now <- getMonotonicTimeNSec
                    I.writeIORef (handleLastRenewed h) now
                Timeout.timeout 750_000 (takeMVar fired)
                    `shouldReturn` Nothing
                Timeout.timeout 1_000_000 (takeMVar fired)
                    `shouldReturn` Just ()

        it "resume also works as tickle" $
            testResume resume

        it "resume also works as tickle with pauses" $
            testResume $ \hndl -> do
                resume hndl
                pause hndl
                resume hndl

        it "old resume did NOT work as tickle" $
            throwsTimeoutThread $
                testResume oldResume
  where
    withHandleTest = withTest withHandle Nothing
    withHandleKillTest = withTest withHandleKillThread
    -- Test that starts with a 'False' IORef and on timeout sets it to true
    withTest withF mRef m f = do
        ref <- maybe freshRef pure mRef
        withF m (I.writeIORef ref True) . f $ refShouldBe ref
    -- run with a 100ms timeout and kill
    runIt f = do
        m <- mkTestManager
        void $ f =<< registerKillThread m (pure ())
    -- Long enough that a short wait does not overrun it on a loaded CI
    -- runner, or with Windows' timer resolution of about 15ms.
    timeoutAmount = 100_000
    mkTestManager = initialize timeoutAmount
    -- Waiting a lot less than the timeout takes
    waitShort = threadDelay $ timeoutAmount `div` 5
    -- Waiting a lot longer than the timeout takes
    waitLong = threadDelay $ timeoutAmount * 5
    -- "resuming" every 2.5ms 20 times
    testResume f = do
        runIt $ \hndl -> do
            forM_ [(1 :: Int) .. 20] $ \_ -> waitShort >> f hndl

mgr1 :: Manager
mgr1 = Manager 1

-- | The cleanup of a fork that takes one runs even when the manager has
--   been stopped and the thread is refused.
itRunsTheCleanupOf
    :: String
    -> (ThreadManager -> String -> IO () -> IO ())
    -- ^ The fork, with its action already given and its cleanup to come
    -> Spec
itRunsTheCleanupOf name fork =
    it ("runs the cleanup of " ++ name) $ do
        tm <- newThreadManager
        stopAfter tm (return ()) $ \_ -> return ()
        done <- newEmptyMVar
        fork tm "probe" $ putMVar done ()
        ranWithin 500_000 done `shouldReturn` True

-- | An exception from the action of a fork that takes a cleanup leaves the
--   thread, after the cleanup has run.
itLetsTheExceptionOutOf
    :: String
    -> (ThreadManager -> String -> IO () -> IO () -> IO ())
    -> Spec
itLetsTheExceptionOutOf name fork =
    it ("lets an exception from the action of " ++ name ++ " out") $ do
        tm <- newThreadManager
        escaped <- newEmptyMVar
        cleaned <- freshRef
        handler0 <- getUncaughtExceptionHandler
        setUncaughtExceptionHandler $ \_ -> void $ tryPutMVar escaped ()
        fork tm "probe" (throwIO $ userError "probe") (I.writeIORef cleaned True)
        out <- ranWithin 500_000 escaped
        setUncaughtExceptionHandler handler0
        out `shouldBe` True
        cleaned `refShouldBe` True

-- | Whether the 'MVar' was filled within the time.
ranWithin :: Int -> MVar () -> IO Bool
ranWithin us var = do
    r <- newEmptyMVar
    void $ forkIO $ takeMVar var >> putMVar r True
    void $ forkIO $ threadDelay us >> putMVar r False
    takeMVar r

freshRef :: IO (IORef Bool)
freshRef = I.newIORef False

refShouldBe :: IORef Bool -> Bool -> IO ()
refShouldBe ref expected =
    I.readIORef ref >>= (`shouldBe` expected)

throwsTimeoutThread :: IO () -> Expectation
throwsTimeoutThread t = t `shouldThrow` (const True :: TimeoutThread -> Bool)

deriving instance Eq Manager
deriving instance Show Manager

-- copied from time-manager-0.3.0 to check it actually is broken
oldResume :: Handle -> IO ()
oldResume h | isEmptyHandle h = return ()
oldResume Handle{..} = do
    key <- EV.registerTimeout handleTimerManager handleTimeout handleAction
    I.writeIORef handleState $ Active key

----------------------------------------------------------------

data Probed

instance WatchdogFor Probed where
    data ContextFor Probed = Probed (IO ())
    decide _ (Probed probe) = return $ setTimeoutProbing 200_000 probe

-- | Watching a task for a second, with a timeout of 0.2 seconds whose
--   probe a peer answers, or not: whether it timed out, and how many
--   probes were sent.
probing :: Bool -> IO (Maybe (), Int)
probing answers = do
    probes <- newIORef (0 :: Int)
    pinged <- newTVarIO False
    let probe = do
            atomicModifyIORef' probes $ \n -> (n + 1, ())
            atomically $ writeTVar pinged True
    wd <- newWatchdog $ Probed probe
    when answers $ void $ forkIO $ forever' $ do
        atomically $ readTVar pinged >>= STM.check >> writeTVar pinged False
        update id wd
    r <- withWatchdog wd (return ()) $
        Timeout.timeout 1_000_000 $ atomically $ timedOutSTM wd
    (,) r <$> readIORef probes
  where
    forever' io = io >> forever' io
