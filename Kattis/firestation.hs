import           Control.Arrow         ((&&&), (***), (>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getElems, thaw)
import           Data.Array.ST         (newArray, readArray, runSTArray,
                                        runSTUArray, writeArray)
import           Data.Array.Unboxed    (Array, UArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: (Int, [Int], Array Int [(Int,Int)]) -> Int
solve (n, fs, graph) = [1..n]
    & map (maximumDistanceFrom &&& id)
    & minimum
    & snd
  where
    maximumDistanceFrom :: Int -> Int
    maximumDistanceFrom u = runST $ do
        dist <- thaw baseline
        writeArray dist u 0
        shortestPath dist (S.singleton u)
        maximum <$> getElems dist

    baseline :: UArray Int Int
    baseline = runSTUArray $ do
        dist <- newArray (1,n) maxBound
        mapM_ (flip (writeArray dist) 0) fs
        shortestPath dist (S.fromList fs)
        pure dist

    shortestPath :: STUArray s Int Int -> S.IntSet -> ST s ()
    shortestPath dist queue = case S.minView queue of
        Nothing         -> pure ()
        Just (x,queue') -> do
            let (du,u) = quotRem x lim
            du' <- readArray dist u
            if du /= du'
                then shortestPath dist queue'
                else do
                    graph ! u
                        & map (\(v,w) -> (du+w,v))
                        & filterM (\(dv,v) -> readArray dist v >>= \dv' -> if dv < dv' then writeArray dist v dv >> pure True else pure False)
                        >>= (foldl' (\q (dv,v) -> S.insert (lim*dv+v) q) queue' >>> shortestPath dist)

lim :: Int
lim = 100000000000

parse :: [[Int]] -> [(Int, [Int], Array Int [(Int,Int)])]
parse []           = []
parse (_:[f,n]:xs) = (n,fs,graph) : parse zs
  where
    (fs,(edges,zs)) = splitAt f xs
        & map last *** span (null >>> not)
    graph = runSTArray $ do
        ret <- newArray (1,n) []
        forM_ edges $ \(u:v:w:_) -> do
            readArray ret u >>= (((v,w):) >>> writeArray ret u)
            readArray ret v >>= (((u,w):) >>> writeArray ret v)
        pure ret

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
