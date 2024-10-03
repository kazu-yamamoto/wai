{-# LANGUAGE OverloadedStrings #-}

module RecvSpec where

import Data.ByteString (ByteString)
import Data.IORef

import Network.Socket.Recv
import Test.Hspec (Spec, describe, hspec, it, shouldReturn)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "RecvN" $ do
        it "should return NBytes " $ do
            testRecvN ["abcde"] 5 "abcde"
            testRecvN ["abcdefgh"] 5 "abcde"
            testRecvN ["ab", "cdefgh"] 5 "abcde"
            testRecvN ["a", "b", "c", "d", "e", "f", "g", "h"] 5 "abcde"

    describe "NBRecvN" $ do
        it "should work well" $ do
            testNBRecvN "" [] 5 [EOF "", EOF "", EOF ""]
            testNBRecvN "" ["abcde"] 5 [NBytes "abcde", EOF ""]
            testNBRecvN
                ""
                ["abcdefgh"]
                5
                [NBytes "abcde", EOF "fgh", EOF ""]

            testNBRecvN
                ""
                ["ab", "cdefgh"]
                5
                [NotEnough, NBytes "abcde", EOF "fgh", EOF ""]
            testNBRecvN
                ""
                ["a", "b", "c", "d", "e", "f", "g", "h"]
                5
                [ NotEnough
                , NotEnough
                , NotEnough
                , NotEnough
                , NBytes "abcde"
                , NotEnough
                , NotEnough
                , NotEnough
                , EOF "fgh"
                , EOF ""
                ]

            testNBRecvN "xyz" [] 2 [NBytes "xy", EOF "z", EOF "", EOF ""]
            testNBRecvN "xyz" [] 5 [EOF "xyz", EOF "", EOF ""]
            testNBRecvN "xyz" ["ab"] 5 [NBytes "xyzab", EOF "", EOF ""]
            testNBRecvN
                "xyz"
                ["abcdefgh"]
                5
                [NBytes "xyzab", NBytes "cdefg", EOF "h", EOF ""]

            testNBRecvN
                "xyz"
                ["ab", "cdefgh"]
                5
                [NBytes "xyzab", NBytes "cdefg", EOF "h", EOF ""]
            testNBRecvN
                "xyz"
                ["a", "b", "c", "d", "e", "f", "g", "h"]
                5
                [ NotEnough
                , NBytes "xyzab"
                , NotEnough
                , NotEnough
                , NotEnough
                , NotEnough
                , NBytes "cdefg"
                , NotEnough
                , EOF "h"
                , EOF ""
                ]

testRecvN :: [ByteString] -> Int -> ByteString -> IO ()
testRecvN xs n res = do
    rcv <- makeRecv xs
    recvN <- makeRecvN "" rcv
    recvN n `shouldReturn` res

testNBRecvN
    :: ByteString -> [ByteString] -> Int -> [NBRecvR] -> IO ()
testNBRecvN ini xs n ress = do
    rcv <- makeRecv xs
    nbRecvN <- makeNBRecvN rcv ini
    mapM_ (nbRecvN n `shouldReturn`) ress

makeRecv :: [ByteString] -> IO Recv
makeRecv xs0 = do
    ref <- newIORef xs0
    return $ rcv ref
  where
    rcv ref = do
        xss <- readIORef ref
        case xss of
            [] -> return ""
            x : xs -> do
                writeIORef ref xs
                return x
