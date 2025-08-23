{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

import           Control.Arrow            ((>>>))
import           Control.Monad            (filterM)
import           Control.Monad.ST         (ST, runST)
import           Data.Array.Unboxed       (UArray, listArray, assocs, bounds, (!))
import           Data.Array.ST            (STUArray, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char                (isControl)
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.List                (find)
import           Data.Maybe               (fromJust)

main :: IO ()
main = do
    [t, n, m] <- C.getLine <&> (C.words >>> map readInt)
    graph <- C.getContents <&> (
                C.filter (isControl >>> not) 
            >>> C.unpack 
            >>> listArray ((1,1),(n,m)))

    let s = assocs graph
            & find (snd >>> (=='S'))
            & fromJust
            & fst
    runST (do seen <- newArray ((1,1),(n,m)) False
              escape 0 t graph seen [s])
        & maybe "NOT POSSIBLE" (show >>> C.pack)
        & C.putStrLn

escape :: Int -> Int -> UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> [(Int,Int)] -> ST s (Maybe Int)
escape curr t graph seen [] = pure Nothing
escape curr t graph seen queue 
    | curr > t      = pure Nothing
    | any out queue = pure (Just curr)
    | otherwise     = mapM neighbours queue >>= (concat >>> escape (curr+1) t graph seen)
    where
        (n,m) = bounds graph & snd
        out (y,x) = y == 1 || x == 1 || y == n || x == m
        neighbours (y,x) = [ ((y+1,x), 'U'), ((y-1,x), 'D'), ((y,x+1), 'L'), ((y,x-1), 'R') ]
                & filter (\(p,c) -> graph ! p == '0' || graph ! p == c)
                & map fst
                & filterM (readArray seen >>> fmap not)
                >>= mapM (\v -> writeArray seen v True >> pure v)
        
readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
