{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.Recv.NonBlocking (
    -- * Non-blocking RecvN
    NBRecvN,
    NBRecvR (..),
    makeNBRecvN,

    -- * Non-blocking RecvMany
    NBRecvManyN,
    makeNBRecvManyN,
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef

import Network.Socket.Recv.Types

makeNBRecvN :: Recv -> IO NBRecvN
makeNBRecvN rcv = nbRecvNRecvN rcv <$> newIORef ""

nbRecvNRecvN :: Recv -> IORef ByteString -> NBRecvN
nbRecvNRecvN _ _ 0 = return $ EOF ""
nbRecvNRecvN rcv ref n = do
    bs0 <- readIORef ref
    let len0 = BS.length bs0
    if
        | len0 == n -> do
            writeIORef ref ""
            return $ NBytes bs0
        | len0 > n -> do
            let (ret, left) = BS.splitAt n bs0
            writeIORef ref left
            return $ NBytes ret
        | otherwise -> do
            bs1 <- rcv
            if BS.null bs1
                then do
                    writeIORef ref ""
                    return $ EOF bs0
                else do
                    let len1 = BS.length bs1
                        bs2 = bs0 `BS.append` bs1
                        len2 = len0 + len1
                    if
                        | len2 == n -> do
                            writeIORef ref ""
                            return $ NBytes bs2
                        | len2 > n -> do
                            let (ret, left) = BS.splitAt n bs2
                            writeIORef ref left
                            return $ NBytes ret
                        | otherwise -> do
                            writeIORef ref bs2
                            return NotEnough

makeNBRecvManyN :: Recv -> ByteString -> IO NBRecvManyN
makeNBRecvManyN rcv "" = nbRecvManyN rcv <$> newIORef (0, id)
makeNBRecvManyN rcv bs0 = nbRecvManyN rcv <$> newIORef (len, (bs0 :))
  where
    len = BS.length bs0

nbRecvManyN
    :: Recv
    -> IORef (Int, [ByteString] -> [ByteString])
    -> NBRecvManyN
nbRecvManyN rcv ref n = do
    (len0, build0) <- readIORef ref
    if
        | len0 == n -> do
            writeIORef ref (0, id)
            return $ NBytes $ build0 []
        | len0 > n -> do
            -- slow path
            let bsx = BS.concat $ build0 []
                (ret, left) = BS.splitAt n bsx
            writeIORef ref (len0 - n, (left :))
            return $ NBytes [ret]
        | otherwise -> do
            bs1 <- rcv
            if BS.null bs1
                then do
                    writeIORef ref (0, id)
                    return $ EOF $ build0 []
                else do
                    let len1 = BS.length bs1
                        len2 = len0 + len1
                    if
                        | len2 == n -> do
                            writeIORef ref (0, id)
                            return $ NBytes $ build0 [bs1]
                        | len2 > n -> do
                            let (bs3, left) = BS.splitAt (n - len0) bs1
                            writeIORef ref (len2 - n, (left :))
                            return $ NBytes $ build0 [bs3]
                        | otherwise -> do
                            writeIORef ref (len2, build0 . (bs1 :))
                            return NotEnough
