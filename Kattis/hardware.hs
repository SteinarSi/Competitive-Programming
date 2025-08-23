{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (getElems, newArray, readArray,
                                        writeArray)
import           Data.Array.ST         (STUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    replicateM_ n $ do
        street <- C.getLine
        order <- C.getLine
        let n = readInt order
        addresses <- getAddresses n

        let counts = runST (newArray (0,9) 0 >>= count addresses)
            makes = counts
                & zipWith (\i c -> C.unwords ["Make", showByteString c, "digit", showByteString i]) [0..]
                & C.unlines
                & C.init
            total = sum counts
            plural | total == 1 = "digit"
                   | otherwise  = "digits"
            summary = C.unwords ["In total", showByteString total, plural]

        C.putStr $ C.unlines [
                street,
                order,
                makes,
                summary
            ]

count :: [Int] -> STUArray s Int Int -> ST s [Int]
count addresses arr = do
    sequence_ $ do
        address <- addresses
        digit <- digits address
        pure (readArray arr digit >>= (succ >>> writeArray arr digit))
    getElems arr

digits :: Int -> [Int]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

getAddresses :: Int -> IO [Int]
getAddresses 0 = pure []
getAddresses n = do
    y:ys <- C.getLine <&> C.words
    if y == "+"
        then let [a,b,c] = map readInt ys
                 addresses = [a,a+c..b]
             in  (addresses ++) <$> getAddresses (n - length addresses)
        else
            (readInt y :) <$> getAddresses (n-1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

showByteString :: Show s => s -> C.ByteString
showByteString = show >>> C.pack
