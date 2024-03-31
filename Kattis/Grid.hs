{-# LANGUAGE TupleSections #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, bounds, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, inRange, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, isDigit)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..), fromList, singleton)

main :: IO ()
main = do
    [n, m] <- C.getLine <&> (C.words >>> map readInt)
    graph <- C.getContents <&> (C.filter isDigit
        >>> C.unpack
        >>> map digitToInt
        >>> listArray ((1,1),(n,m))
        )
    print $ runST (newArray (bounds graph) False >>= bfs graph (singleton (0,(1,1))))

bfs :: UArray (Int,Int) Int -> Seq (Int,(Int,Int)) -> STUArray s (Int,Int) Bool -> ST s Int
bfs _ Empty _ = pure (-1)
bfs graph ((d,(x,y)) :<| rest) seen
    | (x,y) == snd (bounds graph) = pure d
    | otherwise = do
        let k = graph ! (x,y)
        next <- [ (x+k,y), (x-k,y), (x,y+k), (x,y-k) ]
            & filter (inRange (bounds graph))
            & filterM (readArray seen >>> fmap not)
        forM_ next (flip (writeArray seen) True)
        bfs graph (rest <> fromList (map (d+1,) next)) seen

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
