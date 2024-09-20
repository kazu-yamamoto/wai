{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.Recv.Blocking (
    -- * Recv
    Recv,
    defaultRecv,

    -- * RecvN
    RecvN,
    makeDefaultRecvN,
    makeRecvN,

    -- * RecvMany
    RecvMany,

    -- * RecvManyN
    RecvManyN,
    recvManyN,
    recvManyNN,
) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString (..), unsafeCreate)
import Data.IORef
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as N

import Network.Socket.BufferPool.Buffer
import Network.Socket.Recv.Types

defaultRecv :: Socket -> IO ByteString
defaultRecv sock = N.recv sock 2048

----------------------------------------------------------------

-- | Naive implementation for RecvN.
makeDefaultRecvN :: Socket -> IO RecvN
makeDefaultRecvN sock = makeRecvN "" $ defaultRecv sock

----------------------------------------------------------------

-- | This function returns a receiving function
--   based on two receiving functions.
--   The returned function receives exactly N bytes.
--   The first argument is an initial received data.
--   After consuming the initial data, the two functions is used.
--   When N is less than equal to 4096, the buffer pool is used.
--   Otherwise, a new buffer is allocated.
--   In this case, the global lock is taken.
makeRecvN :: ByteString -> Recv -> IO RecvN
makeRecvN bs0 recv = do
    ref <- newIORef bs0
    return $ recvN ref recv

-- | The receiving function which receives exactly N bytes
--   (the fourth argument).
recvN :: IORef ByteString -> Recv -> RecvN
recvN ref recv size = do
    cached <- readIORef ref
    (bs, leftover) <- tryRecvN cached size recv
    writeIORef ref leftover
    return bs

----------------------------------------------------------------

tryRecvN :: ByteString -> Int -> IO ByteString -> IO (ByteString, ByteString)
tryRecvN init0 siz0 recv
    | siz0 <= len0 = return $ BS.splitAt siz0 init0
    | otherwise = go (init0 :) (siz0 - len0)
  where
    len0 = BS.length init0
    go build left = do
        bs <- recv
        let len = BS.length bs
        if len == 0
            then return ("", "")
            else
                if len >= left
                    then do
                        let (consume, leftover) = BS.splitAt left bs
                            ret = concatN siz0 $ build [consume]
                        return (ret, leftover)
                    else do
                        let build' = build . (bs :)
                            left' = left - len
                        go build' left'

----------------------------------------------------------------

concatN :: Int -> [ByteString] -> ByteString
concatN total bss0 = unsafeCreate total $ \ptr -> goCopy bss0 ptr
  where
    goCopy [] _ = return ()
    goCopy (bs : bss) ptr = do
        ptr' <- copy ptr bs
        goCopy bss ptr'

----------------------------------------------------------------

-- Used only in DoH.
-- Recv is getResponseBodyChunk.
-- "lim" is a really limitation
recvManyN :: Recv -> RecvManyN
recvManyN rcv lim = loop id 0
  where
    loop build total = do
        bs <- rcv
        let len = BS.length bs
        if len == 0
            then return (total, build [])
            else do
                let total' = total + len
                    build' = build . (bs :)
                if total' >= lim
                    then return (total', build' [])
                    else loop build' total'

-- Used only in recvVC.
-- "lim" is the size to be received.
recvManyNN :: RecvN -> RecvManyN
recvManyNN rcv lim = loop id 0
  where
    loop build total = do
        let left = lim - total
            siz = min left 2048
        bs <- rcv siz
        let len = BS.length bs
        if len == 0
            then return (total, build [])
            else do
                let total' = total + len
                    build' = build . (bs :)
                if total' >= lim
                    then return (total', build' [])
                    else loop build' total'

----------------------------------------------------------------
