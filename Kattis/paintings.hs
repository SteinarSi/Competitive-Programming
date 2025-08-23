{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative        ((<|>))
import           Control.Arrow              ((&&&), (>>>))
import           Control.Monad              (forM_)
import           Data.Array.ST              (newArray, runSTUArray, writeArray)
import           Data.Array.Unboxed         (UArray, (!))
import           Data.Bool                  (bool)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Function              ((&))
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as S
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust, mapMaybe)
import           Data.Tuple                 (swap)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map C.words
        >>> parse
        >>> map solve
        >>> C.unlines
        >>> C.putStr
    )

solve :: (Int, IM.IntMap C.ByteString, UArray (Int,Int) Bool) -> C.ByteString
solve (n,id2color,banned) = combinations <> "\n" <> favorite
  where
    favorite :: C.ByteString
    favorite = foldr (\x -> (fave [x] (S.fromList ([1..x-1] <> [x+1..n]))<|>)) Nothing [1..n]
        & fromJust
        & reverse
        & map (id2color IM.!)
        & C.unwords
      where
        fave :: [Int] -> S.IntSet -> Maybe [Int]
        fave ps@(p:_) xs
            | S.null xs = Just ps
            | otherwise = S.foldr (\x -> (bool (fave (x:ps) (S.delete x xs)) Nothing (banned!(p,x)) <|>)) Nothing xs

    combinations :: C.ByteString
    combinations = map (\x -> combs x (S.fromList ([1..x-1] <> [x+1..n]))) [1..n]
        & sum
        & show
        & C.pack
      where
        combs :: Int -> S.IntSet -> Int
        combs p xs
            | S.null xs = 1
            | otherwise =  S.foldl' (\r x -> r + bool (combs x (S.delete x xs)) 0 (banned!(p,x))) 0 xs

parse :: [[C.ByteString]] -> [(Int, IM.IntMap C.ByteString, UArray (Int,Int) Bool)]
parse [] = []
parse ([n']:xs:[m]:xss) = (n,id2color,banned) : parse zs
  where
    n = readInt n'
    (ys,zs) = splitAt (readInt m) xss
    (id2color,color2id) = zip [1..] xs
        & IM.fromList &&& (map swap >>> M.fromList)
    banned = runSTUArray $ do
        ret <- newArray ((1,1),(n,n)) False
        forM_ ys $ \[x,y] -> do
            let i = color2id M.! x
                j = color2id M.! y
            writeArray ret (i,j) True
            writeArray ret (j,i) True
        pure ret

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
