{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (>>>))
import           Data.Array.Base       (UArray, indices, listArray, (!))
import           Data.Bits             (shiftR)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let matrix = listArray ((1,1),(n,n)) xs :: UArray (Int,Int) Int

    C.putStr $ case symmetrify matrix False ([],[]) (indices matrix) of
            Nothing             -> "-1\n"
            Just Nothing        -> "1\n" <> format n xs
            Just (Just (xs,ys)) -> "2\n" <> format n xs <> format n ys

symmetrify :: UArray (Int,Int) Int -> Bool -> ([Int],[Int]) -> [(Int,Int)] -> Maybe (Maybe ([Int],[Int]))
symmetrify matrix d (xs,ys) [] | d         = Just (Just (reverse xs, reverse ys))
                               | otherwise = Just Nothing
symmetrify matrix d (xs,ys) ((i,j):is) | odd diff  = Nothing
                                       | otherwise = symmetrify matrix (d || abs ij /= abs ji) (x:xs, y:ys) is
    where
        ij = matrix ! (i,j)
        ji = matrix ! (j,i)
        half = diff `shiftR` 1
        diff = ji - ij
        x    = ij + half
        y    = - half

format :: Int -> [Int] -> C.ByteString
format n = map (show >>> C.pack)
        >>> chunksOf n
        >>> map C.unwords
        >>> C.unlines

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
        & second (chunksOf k)
        & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
