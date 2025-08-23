{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words
            >>> map (C.readInt >>> maybe [1..6] (fst >>> pure))
            >>> solve)
        >>> C.unlines
        >>> C.putStr
    )

solve :: [[Int]] -> C.ByteString
solve [as,bs,cs,ds] | wins == 0 = "0"
                    | wins == total = "1"
                    | otherwise = C.pack (show (wins `div` gcd wins total) <> "/" <> show (total `div` gcd wins total))
    where
        wins = length (filter id games)
        total = length games
        games = do
            a <- as
            b <- bs
            c <- cs
            d <- ds
            pure (score (max a b, min a b) > score (max c d, min c d))

        score :: (Int,Int) -> Int
        score (2,1)             = maxBound
        score (i,j) | i == j    = 1000 * i
                    | otherwise = 10*i + j
