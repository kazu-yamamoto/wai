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
            testNBRecvN [] 5 [EOF "", EOF "", EOF ""]
            testNBRecvN ["abcde"] 5 [NBytes "abcde", EOF ""]
            testNBRecvN ["abcdefgh"] 5 [NBytes "abcde", EOF "fgh", EOF ""]
            testNBRecvN
                ["ab", "cdefgh"]
                5
                [NotEnough, NBytes "abcde", EOF "fgh", EOF ""]
            testNBRecvN
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
    describe "NBRecvManyN" $ do
        it "should work well" $ do
            testNBRecvManyN "" [] 5 [EOF [], EOF [], EOF []]
            testNBRecvManyN "" ["abcde"] 5 [NBytes ["abcde"], EOF []]
            testNBRecvManyN
                ""
                ["abcdefgh"]
                5
                [NBytes ["abcde"], EOF ["fgh"], EOF []]

            testNBRecvManyN
                ""
                ["ab", "cdefgh"]
                5
                [NotEnough, NBytes ["ab", "cde"], EOF ["fgh"], EOF []]
            testNBRecvManyN
                ""
                ["a", "b", "c", "d", "e", "f", "g", "h"]
                5
                [ NotEnough
                , NotEnough
                , NotEnough
                , NotEnough
                , NBytes ["a", "b", "c", "d", "e"]
                , NotEnough
                , NotEnough
                , NotEnough
                , EOF ["f", "g", "h"]
                , EOF []
                ]

testRecvN :: [ByteString] -> Int -> ByteString -> IO ()
testRecvN xs n res = do
    rcv <- makeRecv xs
    recvN <- makeRecvN "" rcv
    recvN n `shouldReturn` res

testNBRecvN :: [ByteString] -> Int -> [NBRecvR ByteString] -> IO ()
testNBRecvN xs n ress = do
    rcv <- makeRecv xs
    nbRecvN <- makeNBRecvN rcv
    mapM_ (nbRecvN n `shouldReturn`) ress

testNBRecvManyN
    :: ByteString -> [ByteString] -> Int -> [NBRecvR [ByteString]] -> IO ()
testNBRecvManyN ini xs n ress = do
    rcv <- makeRecv xs
    nbRecvManyN <- makeNBRecvManyN rcv ini
    mapM_ (nbRecvManyN n `shouldReturn`) ress

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
