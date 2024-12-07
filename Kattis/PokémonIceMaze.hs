{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

import           Control.Arrow            ((>>>), second)
import           Control.Monad            (filterM)
import           Control.Monad.ST         (ST)
import           Data.Array.Base          (UArray, listArray, assocs, elems, (!))
import           Data.Array.ST            (STUArray, readArray, writeArray, newArray, runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char                (isSpace)
import           Data.Function            ((&))
import           Data.Functor             ((<&>))
import           Data.List                (find)
import           Data.Maybe               (fromJust)
import qualified Data.Sequence         as Seq
import           Data.Sequence            (Seq(..))

main :: IO ()
main = do
    [c,r] <- C.getLine <&> (C.words >>> map readInt)
    maze <- C.getContents <&> (C.filter (isSpace >>> not) >>> C.unpack >>> listArray ((1,1),(r,c)))

    let start = assocs maze
            & find (snd >>> (=='M'))
            & fromJust
            & fst
        dist = runSTUArray $ do
            dist <- newArray ((1,1),(r,c)) (-1)
            writeArray dist start 0
            solve maze dist (Seq.singleton (start,0))

    C.putStr (format c dist)

solve :: forall s. UArray (Int,Int) Char -> STUArray s (Int,Int) Int -> Seq ((Int,Int),Int) -> ST s (STUArray s (Int,Int) Int)
solve maze dist Empty = pure dist
solve maze dist ((u,d) :<| xs) = do
        let ds | maze ! u == '_' = filter ((u .-) >>> (maze!) >>> (=='#')) dirs
               | otherwise       = dirs
        ys <- mapM (\d -> slide d (u .+ d)) ds <&> concat
        solve maze dist (xs <> Seq.fromList ys)
    where
        slide :: (Int,Int) -> (Int,Int) -> ST s [((Int,Int),Int)]
        slide dir v = do
            s <- readArray dist v
            case maze ! v of
                '#' -> pure []
                'M' -> pure []
                '.' | s == -1   -> writeArray dist v (d+1) >> pure [(v,d+1)]
                    | otherwise ->                            pure []
                '_' | s == -1   -> writeArray dist v (d+1) >> slide dir (dir .+ v) <&> ((v,d+1):)
                    | otherwise ->                            slide dir (dir .+ v)
    
dirs :: [(Int,Int)]
dirs = [(1,0),(-1,0),(0,1),(0,-1)]

format :: Int -> UArray (Int,Int) Int -> C.ByteString
format c = elems
        >>> map (show >>> C.pack)
        >>> chunksOf c
        >>> map C.unwords
        >>> C.unlines

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
        & second (chunksOf k)
        & uncurry (:)

(.+) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(.+) (a,b) (x,y) = (a+x, b+y)

(.-) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(.-) (a,b) (x,y) = (a-x, b-y)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
