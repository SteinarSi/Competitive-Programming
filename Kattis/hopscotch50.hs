{-# LANGUAGE TupleSections #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Data.Array            (Array, array, (!))
import           Data.Array.Base       (MArray, elems, newArray, readArray,
                                        writeArray)
import           Data.Array.ST         (STArray, runSTArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix, range)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)

    let positions = runSTArray $ do
            arr <- newArray (1, k) []
            forM_ (zip (range ((1,1),(n,n))) xs) $ \(pos, c) -> modifyArray arr c (pos:)
            pure arr

    print (solve k positions)

solve :: Int -> Array Int [(Int,Int)] -> Int
solve k positions | 0 `elem` lens = -1
                  | otherwise = [1..length (positions ! 1)]
                        & map ((1,) >>> (opt!))
                        & minimum
    where
        lens = map length (elems positions)
        m = maximum lens
        opt = array ((1,1),(k,m)) $ map ((k,) >>> (,0)) [1..m] ++ (do
                l <- [k-1,k-2..1]
                (i,p) <- zip [1..] (positions ! l)

                let dist = positions ! (l+1)
                        & zipWith (\j q -> opt ! (l + 1, j) + manhattan p q) [1..]
                        & minimum

                pure ((l,i), dist)
            )

manhattan :: (Int,Int) -> (Int,Int) -> Int
manhattan (a,b) (x,y) = abs (a-x) + abs (b-y)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= writeArray arr ix . f
