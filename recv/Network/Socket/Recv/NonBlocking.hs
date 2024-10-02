{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.Recv.NonBlocking (
    -- * Non-blocking RecvN
    NBRecvN,
    NBRecvR (..),
    makeNBRecvN,
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef

import Network.Socket.Recv.Types

data State = E | S ByteString | M ([ByteString] -> [ByteString])

makeNBRecvN :: Recv -> ByteString -> IO NBRecvN
makeNBRecvN rcv "" = nbRecvN rcv <$> newIORef (0, E)
makeNBRecvN rcv bs0 = nbRecvN rcv <$> newIORef (len, S bs0)
  where
    len = BS.length bs0

nbRecvN
    :: Recv
    -> IORef (Int, State)
    -> NBRecvN
nbRecvN rcv ref n = do
    (len0, st) <- readIORef ref
    if
        | len0 == n -> do
            writeIORef ref (0, E)
            case st of
                E -> return $ NBytes []
                S bs0 -> return $ NBytes [bs0]
                M build0 -> return $ NBytes $ build0 []
        | len0 > n -> do
            case st of
                E -> error "nbRecvN E"
                S bs0 -> do
                    let (ret, left) = BS.splitAt n bs0
                    writeIORef ref (BS.length left, S left)
                    return $ NBytes [ret]
                M build0 -> do
                    -- slow path
                    let bs = BS.concat $ build0 []
                        (ret, left) = BS.splitAt n bs
                    writeIORef ref (BS.length left, S left)
                    return $ NBytes [ret]
        | otherwise -> do
            bs1 <- rcv
            if BS.null bs1
                then do
                    writeIORef ref (0, E)
                    case st of
                        E -> return $ EOF []
                        S bs -> return $ EOF [bs]
                        M build -> return $ EOF $ build []
                else do
                    let len1 = BS.length bs1
                        len2 = len0 + len1
                    if
                        | len2 == n -> do
                            writeIORef ref (0, E)
                            case st of
                                E -> return $ NBytes [bs1]
                                S bs0 -> return $ NBytes [bs0, bs1]
                                M build0 -> return $ NBytes $ build0 [bs1]
                        | len2 > n -> do
                            let (bs3, left) = BS.splitAt (n - len0) bs1
                            writeIORef ref (BS.length left, S left)
                            case st of
                                E -> return $ NBytes [bs3]
                                S bs0 -> return $ NBytes [bs0, bs3]
                                M build0 -> return $ NBytes $ build0 [bs3]
                        | otherwise -> do
                            case st of
                                E -> writeIORef ref (len2, S bs1)
                                S bs0 -> writeIORef ref (len2, M ((bs0 :) . (bs1 :)))
                                M build0 -> writeIORef ref (len2, M (build0 . (bs1 :)))
                            return NotEnough
