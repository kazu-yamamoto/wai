{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module SharedListenerSpec (spec) where

import Test.Hspec
#ifndef WINDOWS
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (bracket)
import Network.HTTP.Types (status200)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp
import System.Posix.IO (dup)
import System.Posix.Types (Fd (..))
import System.Timeout (timeout)

spec :: Spec
spec = describe "a listening socket shared by two accept loops" $ do
    -- As after a fork: two descriptors for one socket.  Both loops wake
    -- for the connection and one takes it; the other used to wait in
    -- accept for the next one, and did not stop until it came.
    it "stops both, the one that missed the connection too" $ do
        bracket openFreePort (close . snd) $ \(testPort, sock) -> do
            sock2 <- withFdSocket sock $ \fd -> do
                Fd fd2 <- dup (Fd fd)
                mkSocket fd2
            stop1 <- newEmptyMVar
            stop2 <- newEmptyMVar
            let settings slot =
                    setInstallShutdownHandler (putMVar slot) $
                        setGracefulShutdownTimeout (Just 1) defaultSettings
                app _ respond = respond $ responseLBS status200 [("Connection", "close")] "ok"
            withAsync (runSettingsSocket (settings stop1) sock app) $ \server1 ->
                withAsync (runSettingsSocket (settings stop2) sock2 app) $ \server2 -> do
                    s1 <- takeMVar stop1
                    s2 <- takeMVar stop2
                    threadDelay 100_000
                    client <- socket AF_INET Stream defaultProtocol
                    connect client $ SockAddrInet (fromIntegral testPort) (tupleToHostAddress (127, 0, 0, 1))
                    sendAll client "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                    _ <- recv client 4096
                    close client
                    threadDelay 100_000
                    s1 >> s2
                    timeout 3_000_000 (wait server1) `shouldReturn` Just ()
                    timeout 3_000_000 (wait server2) `shouldReturn` Just ()
#else
spec :: Spec
spec = return ()
#endif
