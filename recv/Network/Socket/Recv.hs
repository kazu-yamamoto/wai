module Network.Socket.Recv (
    -- * Recv
    Recv,
    defaultRecv,

    -- * RecvMany
    RecvMany,

    -- * RecvN
    RecvN,
    makeDefaultRecvN,
    makeRecvN,

    -- * RecvManyN
    RecvManyN,
    recvManyN,
    recvManyNN,

    -- * Non-blocking RecvN
    NBRecvN,
    NBRecvR (..),
    makeNBRecvN,
)
where

import Network.Socket.Recv.Blocking
import Network.Socket.Recv.NonBlocking
