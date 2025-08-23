{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (sort, sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (..))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt)
        >>> split
        >>> zip [1..]
        >>> mapM_ (fmap solve
            >>> format
            >>> C.putStrLn)
    )

solve :: ([Int],[Int]) -> Int
solve (xs,ys) = zipWith (*) (sort xs) (sortOn Down ys) & sum

format :: (Int, Int) -> C.ByteString
format (i, msp) = "Case #" <> C.pack (show i) <> ": " <> C.pack (show msp)

split :: [[Int]] -> [([Int],[Int])]
split []            = []
split (_:xs:ys:xss) = (xs,ys) : split xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
