{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow            ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char                (toUpper)
import           Data.Function            ((&))
import qualified Data.IntMap           as M

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words 
            >>> map convert 
            >>> C.unwords 
            >>> capitalize)
        >>> C.unlines
        >>> C.putStr
    )

capitalize :: C.ByteString -> C.ByteString
capitalize x | C.null x  = x
             | otherwise = toUpper (C.head x) `C.cons` C.tail x

convert :: C.ByteString -> C.ByteString
convert x = case C.readInt x of
    Nothing    -> x
    Just (n,_) -> numbers M.! n

numbers :: M.IntMap C.ByteString
numbers = "zero" : ones ++ teens ++ concatMap (\t -> t : map ((t <> "-") <>) ones) tens
        & zip [0..]
        & M.fromAscList
    where
        ones :: [C.ByteString]
        ones = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

        teens :: [C.ByteString]
        teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

        tens :: [C.ByteString]
        tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
