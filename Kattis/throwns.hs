{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    xs <- C.getContents <&> C.words
    print (solve n [0] xs)

solve :: Int -> [Int] -> [C.ByteString] -> Int
solve n (x:_) []         = x
solve n xs ("undo":c:cs) = solve n (drop (readInt c) xs) cs
solve n (x:xs) (c:cs)    = solve n ((x + readInt c) `mod` n : x : xs) cs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
