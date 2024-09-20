{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.Recv.NonBlocking (
    -- * Non-blocking RecvN
    NBRecvN,
    NBRecvR (..),
    makeDefaultNBRecvN,

    -- * Non-blocking RecvMany
    NBRecvMany,

    -- * Non-blocking RecvManyN
    NBRecvManyN,
    NBRecvManyR (..),
    nbRecvManyNN,
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as N

import Network.Socket.Recv.Types

makeDefaultNBRecvN :: Socket -> IO NBRecvN
makeDefaultNBRecvN sock = do
    ref <- newIORef ""
    return $ defaultNBRecvN sock ref

defaultNBRecvN :: Socket -> IORef ByteString -> NBRecvN
defaultNBRecvN _ _ 0 = return $ EOF ""
defaultNBRecvN s ref n = do
    bs0 <- readIORef ref
    let len0 = BS.length bs0
    if len0 >= n
        then do
            let (bs1, bs2) = BS.splitAt n bs0
            writeIORef ref bs2
            return $ NBytes bs1
        else do
            let n1 = n - BS.length bs0
            bs1 <- N.recv s n1
            if BS.null bs1
                then do
                    writeIORef ref ""
                    return $ EOF bs0
                else do
                    let bs2 = bs0 `BS.append` bs1
                        len2 = BS.length bs2
                    if len2 == n
                        then do
                            writeIORef ref ""
                            return $ NBytes bs2
                        else do
                            writeIORef ref bs2
                            return NotEnough

nbRecvManyNN :: NBRecvN -> NBRecvManyN
nbRecvManyNN = undefined
