{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))

main :: IO ()
main = C.getLine >>= (solve >>> putStrLn)

solve :: C.ByteString -> String
solve xs = show (dp ! (0,n))
    where
        n = C.length xs
        rng = ((0,0),(n,n))

        dp :: Array (Int,Int) Integer
        dp = listArray rng (map f (range rng))

        f :: (Int,Int) -> Integer
        f (i,j) | i == j = 1
                | odd i /= odd j = 0
                | otherwise = sum [ m i k * dp ! (i+1,k) * dp ! (k+1,j) | k <- [i+1,i+3 .. j-1] ]

        m :: Int -> Int -> Integer
        m i k = case (C.index xs i, C.index xs k) of
            ('(',')')                     -> 1
            ('[',']')                     -> 1
            ('{','}')                     -> 1
            ('<','>')                     -> 1
            ('?','?')                     -> 4
            ('?', b ) | b `C.elem` ")]}>" -> 1
            ( b ,'?') | b `C.elem` "<{[(" -> 1
            ( _ , _ )                     -> 0
