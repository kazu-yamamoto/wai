{-# LANGUAGE OverloadedStrings #-}

-- | Collection for network receive functions.
module Network.Socket.BufferPool.Recv (
    receive,
) where

import Network.Socket (Socket, recvBuf)

import Network.Socket.BufferPool.Buffer
import Network.Socket.BufferPool.Types
import Network.Socket.Recv.Types

----------------------------------------------------------------

-- | The receiving function with a buffer pool.
--   The buffer pool is automatically managed.
receive :: Socket -> BufferPool -> Recv
receive sock pool = withBufferPool pool $ \ptr size -> recvBuf sock ptr size
