{-# LANGUAGE LambdaCase #-}

import           Control.Arrow         ((***), (>>>))
import           Data.Array.Unboxed    (Array, UArray, assocs, inRange,
                                        listArray, range, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find, permutations)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    r:c:rest <- C.getContents <&> C.words
    let
        rng = ((1,1),(readInt r, readInt c))
        n = C.length (last rest)

        grid :: UArray (Int,Int) Char
        grid = listArray rng (concatMap C.unpack (init rest))

        Just (s,_) = find (snd >>> (=='S')) (assocs grid)

        moves = last rest
            & C.unpack
            & map (\case 'N' -> (-1,0); 'S' -> (1,0); 'W' -> (0,-1); 'E' -> (0,1))

        movesA :: Array Int (Int,Int)
        movesA = listArray (0,n-1) moves

        positions :: Array Int (Int,Int)
        positions = listArray (0,n) (scanl (\(y,x) -> (y+)***(x+)) s moves)

        perms :: Array Int [(Int,Int)]
        perms = listArray (1,24) (permutations [(-1,0),(1,0),(0,-1),(0,1)])

        dp :: Array (Int,Int) Int
        dp = listArray dprng (map f (range dprng))
          where
            dprng = ((0,1),(n,24))

            f :: (Int,Int) -> Int
            f (0,_) = 0
            f (i,p) = assocs perms
                & filter (snd >>> predict (i-1) >>> (== movesA ! (i-1)))
                & map (\(q,_) -> dp ! (i-1,q) + bool 0 1 (p/=q))
                & minimum

        predict :: Int -> [(Int,Int)] -> (Int,Int)
        predict i = filter (((y+)***(x+)) >>> \v -> inRange rng v && grid ! v /= '#') >>> head
          where
            (y,x) = positions ! i

    [1..24]
        & map ((n,) >>> (dp!))
        & minimum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
