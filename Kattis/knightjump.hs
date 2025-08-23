{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad         (filterM, forM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, inRange, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, assocs, bounds, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

import           Control.Arrow         ((***), (>>>))
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)

main :: IO ()
main = do
    (n, xs) <- C.getContents <&> (C.readInt >>> fromJust)
    let board = C.filter (`C.elem` ".#K") xs
            & C.unpack
            & listArray ((1,1),(n,n)) :: UArray (Int,Int) Char
        s = assocs board
            & find (snd >>> (=='K'))
            & fromJust
            & fst

    print $ runST (newArray ((1,1),(n,n)) False >>= solve 0 board [s])

solve :: Int -> UArray (Int,Int) Char -> [(Int,Int)] -> STUArray s (Int,Int) Bool -> ST s Int
solve gen board queue seen | null queue = pure (-1)
                           | (1,1) `elem` queue = pure gen
                           | otherwise = do
                                next <- concat <$> forM queue (\(y,x) -> dirs
                                        & map ((y+) *** (x+))
                                        & filter (inRange (bounds board))
                                        & filter ((board !) >>> (/='#'))
                                        & filterM (readArray seen >>> fmap not)
                                        >>= mapM (\v -> writeArray seen v True >> pure v))
                                solve (gen+1) board next seen

dirs :: [(Int, Int)]
dirs = [(1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2)]
