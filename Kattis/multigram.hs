{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            ((>>>))
import           Data.Array.Unboxed       (UArray, listArray, (!), bounds)
import qualified Data.ByteString.Char8 as C
import           Data.Char                (ord, isAlpha)
import           Data.Function            ((&))
import           Data.List                (nub, sort, find)

main :: IO ()
main = C.getLine >>= (solulu >>> C.putStrLn)

solulu :: C.ByteString -> C.ByteString
solulu xs = [1..n`div`2]
        & filter potential
        & find valid
        & maybe "-1" (`C.take` xs)
    where
        n = C.length xs

        hash :: UArray Int Int
        hash = xs
            & C.foldl' (\(y:ys) x -> (y + hashChar x) `mod` base : y : ys) [0]
            & reverse
            & listArray (0,n)

        potential :: Int -> Bool
        potential k | n `mod` k /= 0 = False
                    | otherwise      = [k,k+k..n]
                                           & map (\i -> (hash ! i - hash ! (i-k)) `mod` base)
                                           & nub
                                           & tail
                                           & null

        valid :: Int -> Bool
        valid k = all (substr >>> (==substr 0)) [k,2*k..n-k-1]
            where
                substr :: Int -> String
                substr i = map (C.index xs) [i..i+k-1]
                            & sort

{-# INLINE base #-}
base :: Int
base = 2^62

{-# INLINE hashChar #-}
hashChar :: Char -> Int
hashChar = ord >>> subtract (ord 'a') >>> (coefficients !)
    where
        coefficients :: UArray Int Int
        coefficients = listArray (0,26) [100183, 108127, 119929, 130729, 145991, 156493, 178831, 181459, 182537, 194863, 194917, 197207, 198529, 199601, 203227, 205883, 208141, 209519, 211781, 212981, 214631, 97463, 95881, 93997, 91393, 89519, 86857]

