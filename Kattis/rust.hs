{-# LANGUAGE BangPatterns #-}

import           Control.Category      ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, range, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, isDigit, isSpace)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,k] <- C.getLine <&> (C.words >>> map readInt)
    graph <- C.getContents <&> (C.filter (isSpace >>> not) >>> C.unpack >>> listArray ((1,1),(n,n)))
    print $ solulu n k graph

solulu :: Int -> Int -> UArray (Int,Int) Char -> Int
solulu n k graph = startSlide 0 (1,1)
    where
        startSlide :: Int -> (Int,Int) -> Int
        startSlide !best (y,x) | y+k-1 > n = best
                               | x+k-1 > n = startSlide best (y+1,1)
                               | otherwise = case find (\x' -> graph ! (y,x') /= '.' || graph ! (y+k-1,x') /= '.') [x+k-1,x+k-2..x] of
                Just x' -> startSlide best (y,x'+1)
                Nothing | any (\y' -> graph ! (y',x) /= '.' || graph ! (y',x+k-1) /= '.') [y+1..y+k-2] -> startSlide best (y,x+1)
                        | otherwise -> slideRight (max best value) value (y,x)
                where value = range ((y+1,x+1),(y+k-2,x+k-2))
                        & map (graph !)
                        & filter isDigit
                        & map digitToInt
                        & sum

        slideRight :: Int -> Int -> (Int,Int) -> Int
        slideRight !best !curr (y,x) | x+k > n = startSlide best (y+1,1)
                                     | graph ! (y,x+k) /= '.' || graph ! (y+k-1,x+k) /= '.' = startSlide best (y,x+k+1)
                                     | otherwise = slideRight best' curr' (y,x+1)
                where
                    colValue x' = [graph ! (y',x') | y' <- [y+1..y+k-2]]
                        & filter isDigit
                        & map digitToInt
                        & sum
                    gain = colValue (x+k)
                    loss = colValue (x+1)
                    curr' = curr + gain - loss
                    best' | any (\y' -> graph ! (y',x+1) /= '.' || graph ! (y',x+k) /= '.') [y+1..y+k-2] = best
                          | otherwise = max best curr'

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
