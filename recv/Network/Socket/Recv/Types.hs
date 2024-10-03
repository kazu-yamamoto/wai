module Network.Socket.Recv.Types where

import Data.ByteString (ByteString)

-- | Type for the receiving function with a buffer pool.
type Recv = IO ByteString

-- | Type for the receiving function which receives N bytes.
type RecvN = Int -> IO ByteString

type RecvMany = IO (Int, [ByteString])

type RecvManyN = Int -> IO (Int, [ByteString])

data NBRecvR = EOF ByteString | NotEnough | NBytes ByteString
    deriving (Eq, Show)

type NBRecvN = Int -> IO NBRecvR
