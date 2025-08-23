{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            ((>>>), (***), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&), on)
import           Data.List                (maximumBy)
import           Data.Maybe               (fromJust)
import           Data.Ratio               ((%))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words 
            >>> map readInt 
            >>> head &&& last)
        >>> split
        >>> map (uncurry solve)
        >>> C.unlines
        >>> C.putStr
    )

solve :: Int -> [(Int,Int)] -> C.ByteString
solve m xs = case filter (fst >>> (<=m)) xs of
                 [] -> "No suitable tickets offered"
                 ys -> let (a,b) = maximumBy (compare `on` (uncurry (%) &&& fst)) ys
                       in  C.concat ["Buy ", C.pack (show a), " tickets for $", C.pack (show b)]

split :: [(Int,Int)] -> [(Int, [(Int,Int)])]
split [] = []
split [(0,0)] = []
split ((n,m):xs) = splitAt n xs
        & (,) m *** split
        & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
