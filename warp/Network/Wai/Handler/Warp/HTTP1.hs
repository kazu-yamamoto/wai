{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Handler.Warp.HTTP1 (
    http1
  , http1server
  ) where

import "iproute" Data.IP (toHostAddress, toHostAddress6)
import qualified Control.Concurrent as Conc (yield)
import Control.Exception as E
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network.Socket (SockAddr(SockAddrInet, SockAddrInet6))
import Network.Wai
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import qualified System.TimeManager as T

import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Imports hiding (readInt)
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Response
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

http1 :: Settings -> InternalInfo -> Connection -> Transport -> Application -> SockAddr -> T.Handle -> ByteString -> IO ()
http1 settings ii conn transport app origAddr th bs0 = do
    istatus <- newIORef True
    src <- mkSource (wrappedRecv conn istatus (settingsSlowlorisSize settings))
    addr <- getProxyProtocolAddr src
    let h1conf = H1Config {
            sendR = \req idxhdr rsp -> do
                  T.resume th
                  -- FIXME consider forcing evaluation of the res here to
                  -- send more meaningful error messages to the user.
                  -- However, it may affect performance.
                  writeIORef istatus False
                  sendResponse settings conn ii th (readSource src) req idxhdr rsp
          , recvR = \fstreq -> do
                req <- recvRequest settings conn ii th addr src transport fstreq
                -- Let the application run for as long as it wants
                T.pause th
                return req
          , sendE = sendErrorResponse settings conn ii th istatus
          , tailC = tailCheck settings th
          , cAddr = addr
          }
    leftoverSource src bs0
    http1server settings h1conf app
  where
    wrappedRecv Connection { connRecv = recv } istatus slowlorisSize = do
        bs <- recv
        unless (BS.null bs) $ do
            writeIORef istatus True
            when (BS.length bs >= slowlorisSize) $ T.tickle th
        return bs

    getProxyProtocolAddr src =
        case settingsProxyProtocol settings of
            ProxyProtocolNone ->
                return origAddr
            ProxyProtocolRequired -> do
                seg <- readSource src
                parseProxyProtocolHeader src seg
            ProxyProtocolOptional -> do
                seg <- readSource src
                if BS.isPrefixOf "PROXY " seg
                    then parseProxyProtocolHeader src seg
                    else do leftoverSource src seg
                            return origAddr

    parseProxyProtocolHeader src seg = do
        let (header,seg') = BS.break (== 0x0d) seg -- 0x0d == CR
            maybeAddr = case BS.split 0x20 header of -- 0x20 == space
                ["PROXY","TCP4",clientAddr,_,clientPort,_] ->
                    case [x | (x, t) <- reads (decodeAscii clientAddr), null t] of
                        [a] -> Just (SockAddrInet (readInt clientPort)
                                                       (toHostAddress a))
                        _ -> Nothing
                ["PROXY","TCP6",clientAddr,_,clientPort,_] ->
                    case [x | (x, t) <- reads (decodeAscii clientAddr), null t] of
                        [a] -> Just (SockAddrInet6 (readInt clientPort)
                                                        0
                                                        (toHostAddress6 a)
                                                        0)
                        _ -> Nothing
                ("PROXY":"UNKNOWN":_) ->
                    Just origAddr
                _ ->
                    Nothing
        case maybeAddr of
            Nothing -> throwIO (BadProxyHeader (decodeAscii header))
            Just a -> do leftoverSource src (BS.drop 2 seg') -- drop CRLF
                         return a

    decodeAscii = map (chr . fromEnum) . BS.unpack

----------------------------------------------------------------

data H1Config = H1Config {
    recvR :: Bool -> IO (Request, Maybe (IORef Int), IndexedHeader, IO ByteString)
  , sendR :: Request -> IndexedHeader -> Response -> IO Bool

  , sendE :: Request -> SomeException -> IO Bool
  , tailC :: IO ByteString -> Maybe (IORef Int) -> Bool -> IO Bool
  , cAddr :: SockAddr
  }

----------------------------------------------------------------

http1server :: Settings -> H1Config -> Application -> IO ()
http1server settings h1conf@H1Config{..} app =
    loop True `E.catch` handler
  where
    handler e
      -- See comment below referencing
      -- https://github.com/yesodweb/wai/issues/618
      | Just NoKeepAliveRequest <- fromException e = return ()
      -- No valid request
      | Just (BadFirstLine _)   <- fromException e = return ()
      | otherwise = do
          _ <- sendE defaultRequest { remoteHost = cAddr } e
          throwIO e

    loop firstRequest = do
        keepAlive <- processRequest settings h1conf app firstRequest

        -- When doing a keep-alive connection, the other side may just
        -- close the connection. We don't want to treat that as an
        -- exceptional situation, so we pass in False to http1 (which
        -- in turn passes in False to recvRequest), indicating that
        -- this is not the first request. If, when trying to read the
        -- request headers, no data is available, recvRequest will
        -- throw a NoKeepAliveRequest exception, which we catch here
        -- and ignore. See: https://github.com/yesodweb/wai/issues/618

        when keepAlive $ loop False

----------------------------------------------------------------

processRequest :: Settings -> H1Config -> Application -> Bool -> IO Bool
processRequest settings H1Config{..} app firstRequest = do
    (req,mremainingRef,idxhdr,nextBodyFlush) <- recvR firstRequest

    E.handle (handler req) $ do
        -- In the event that some scarce resource was acquired during
        -- creating the request, we need to make sure that we don't get
        -- an async exception before calling the ResponseSource.
        keepAliveRef <- newIORef $ error "keepAliveRef not filled"
        r <- E.try $ app req $ \rsp -> do
            keepAlive <- sendR req idxhdr rsp
            writeIORef keepAliveRef keepAlive
            return ResponseReceived
        case r of
            Right ResponseReceived -> return ()
            Left e@(SomeException _)
              | Just (ExceptionInsideResponseBody e') <- fromException e -> throwIO e'
              | otherwise -> do
                    keepAlive <- sendE req e
                    settingsOnException settings (Just req) e
                    writeIORef keepAliveRef keepAlive

        keepAlive <- readIORef keepAliveRef

        -- We just send a Response and it takes a time to
        -- receive a Request again. If we immediately call recv,
        -- it is likely to fail and cause the IO manager to do some work.
        -- It is very costly, so we yield to another Haskell
        -- thread hoping that the next Request will arrive
        -- when this Haskell thread will be re-scheduled.
        -- This improves performance at least when
        -- the number of cores is small.
        Conc.yield
        tailC nextBodyFlush mremainingRef keepAlive
  where
    handler req e = do
        settingsOnException settings (Just req) e
        -- Don't throw the error again to prevent calling settingsOnException twice.
        return False

----------------------------------------------------------------

tailCheck :: Settings -> T.Handle -> IO ByteString -> Maybe (IORef Int) -> Bool -> IO Bool
tailCheck _ _ _ _                                False = return False
tailCheck settings th nextBodyFlush mremainingRef True = do
    -- If there is an unknown or large amount of data to still be read
    -- from the request body, simple drop this connection instead of
    -- reading it all in to satisfy a keep-alive request.
    case settingsMaximumBodyFlush settings of
        Nothing -> do
            flushEntireBody nextBodyFlush
            T.resume th
            return True
        Just maxToRead -> do
            let tryKeepAlive = do
                    -- flush the rest of the request body
                    isComplete <- flushBody nextBodyFlush maxToRead
                    if isComplete then do
                        T.resume th
                        return True
                      else
                        return False
            case mremainingRef of
                Just ref -> do
                    remaining <- readIORef ref
                    if remaining <= maxToRead then
                        tryKeepAlive
                      else
                        return False
                Nothing -> tryKeepAlive

flushEntireBody :: IO ByteString -> IO ()
flushEntireBody src =
    loop
  where
    loop = do
        bs <- src
        unless (BS.null bs) loop

flushBody :: IO ByteString -- ^ get next chunk
          -> Int -- ^ maximum to flush
          -> IO Bool -- ^ True == flushed the entire body, False == we didn't
flushBody src = loop
  where
    loop toRead = do
        bs <- src
        let toRead' = toRead - BS.length bs
        case () of
            ()
                | BS.null bs -> return True
                | toRead' >= 0 -> loop toRead'
                | otherwise -> return False

----------------------------------------------------------------

sendErrorResponse :: Settings -> Connection -> InternalInfo -> T.Handle -> IORef Bool -> Request -> SomeException -> IO Bool
sendErrorResponse settings conn ii th istatus req e = do
    status <- readIORef istatus
    if shouldSendErrorResponse e && status then
        sendResponse settings conn ii th (return BS.empty) req defaultIndexRequestHeader errorResponse
      else
        return False
  where
    shouldSendErrorResponse se
      | Just ConnectionClosedByPeer <- fromException se = False
      | otherwise                                       = True
    errorResponse = settingsOnExceptionResponse settings e
