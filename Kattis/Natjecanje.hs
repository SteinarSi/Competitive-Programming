{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    missing <- C.getLine <&> (C.words >>> map readInt)
    spare   <- C.getLine <&> (C.words >>> map readInt)

    print $ runST $ do
            miss <- newArray (1,n) False
            forM_ missing (flip (writeArray miss) True)
            solve n spare miss

solve :: Int -> [Int] -> STUArray s Int Bool ->  ST s Int
solve n spare missing = do
    heuristic spare >>= rest
    getElems missing <&> (filter id >>> length)

    where
        heuristic []     = pure []
        heuristic (1:xs) = writeArray missing 2 False >> heuristic xs
        heuristic (x:xs)
            | x == n = writeArray missing (x-1) False >> heuristic xs
            | otherwise = do
                left <- readArray missing (x-1)
                right <- readArray missing (x+1)
                if
                    | left && not right -> writeArray missing (x-1) False >> heuristic xs
                    | right && not left -> writeArray missing (x+1) False >> heuristic xs
                    | otherwise -> (x:) <$> heuristic xs

        rest [] = pure ()
        rest (x:xs) = do
            left <- readArray missing (x-1)
            right <- readArray missing (x+1)
            if
                | left      -> writeArray missing (x-1) False >> rest xs
                | right     -> writeArray missing (x+1) False >> rest xs
                | otherwise -> rest xs


readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
