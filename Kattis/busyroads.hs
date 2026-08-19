{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, (!))
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        runSTArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    [n,m,c]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            ret <- newArray (1,n) []
            forM_ xs $ \[a,b,t,l,r] -> do
                readArray ret a >>= (((b,t,l,r):) >>> writeArray ret a)
                readArray ret b >>= (((a,t,l,r):) >>> writeArray ret b)
            pure ret

    print $ runST $ do
        dist <- newArray (1,n) maxBound
        solve n c graph dist (S.singleton (0,1))

solve :: Int -> Int -> Array Int [(Int,Int,Int,Int)] -> STUArray s Int Int -> S.Set (Int,Int) -> ST s Int
solve n c graph dist queue = do
    let ((s',a),queue') = S.deleteFindMin queue
        (d,s) = quotRem s' c
    d' <- readArray dist a
    if
        | a == n -> pure s'
        | d > d' -> solve n c graph dist queue'
        | otherwise -> graph ! a
            & map (\(b,t,l,r) -> if
                | s < l     -> (c*d + l + t, b)
                | s > r     -> (c*(d+1) + l + t, b)
                | otherwise -> (c*d + s + t, b))
            & filterM (\(s'',b) -> readArray dist b >>= \s''' -> if s'' < s''' then writeArray dist b s'' >> pure True else pure False)
            >>= (foldr S.insert queue' >>> solve n c graph dist)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
