{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import           Control.Arrow            ((>>>))
import           Data.Maybe               (fromJust)
import           Data.Functor             ((<&>))
import           Data.Array               (Array)
import           Data.Array.Base          (UArray, listArray, (!), array, elems)
import           Data.Bits                (shiftL, shiftR)
import           Control.Monad            (forM_)
import           Data.Function            ((&))

main :: IO ()
main = do
    (n:_):xs:qs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let jumps = lca n (monotonic [] [] n (listArray (1,n) xs))

    map (\(s:d:_) -> jump jumps 0 s d) qs
        & C.unlines
        & C.putStr

jump :: UArray (Int,Int) Int -> Int -> Int -> Int -> C.ByteString
jump jumps pow s d | s == 0 = "leik lokid"
                   | d == 0 = C.pack (show s)
                   | otherwise = jump jumps (pow+1) s' (shiftR d 1)
    where
        s' | odd d = jumps ! (s, pow)
           | otherwise = s

lca :: Int -> UArray Int Int -> UArray (Int,Int) Int
lca n right = jumps 
        & elems 
        & listArray ((1,0), (n,m))
    where
        m = findPow n 1 - 1

        jumps :: Array (Int,Int) Int
        jumps = listArray ((1,0), (n,m)) [next i j | i <- [1..n], j <- [0..m]]

        next :: Int -> Int -> Int
        next i 0 = right ! i
        next i j | jumps ! (i,j-1) == 0 = 0
                 | otherwise = jumps ! (jumps ! (i,j-1), j-1)

monotonic :: [Int] -> [Int] -> Int -> UArray Int Int -> UArray Int Int
monotonic ret stack 0 xs = listArray (1,length ret) ret
monotonic ret stack i xs = monotonic (j':ret) stack' (pred i) xs
    where
        x = xs ! i
        (j', stack') = popLower stack

        popLower :: [Int] -> (Int, [Int])
        popLower [] = (0, [i])
        popLower (j:js) | xs ! j < x = popLower js
                        | otherwise  = (j, i:j:js)

findPow :: Int -> Int -> Int
findPow n m | shiftL 1 m > n+1 = m
            | otherwise        = findPow n (m+1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
