{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort, sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    g <- C.getLine <&> readInt
    m <- C.getContents <&> (
            C.lines
        >>> map (C.words >>> map readInt >>> (\(start:end:_) -> (end,start)))
        >>> sort
        >>> maxIntervalIndependentSet 0 0
        )
    C.putStrLn $ if m >= g
        then "YES"
        else "NO"

maxIntervalIndependentSet :: Int -> Int -> [(Int,Int)] -> Int
maxIntervalIndependentSet !ret prev [] = ret
maxIntervalIndependentSet !ret prev ((end,start):xs)
    | start >= prev = maxIntervalIndependentSet (ret+1) end xs
    | otherwise     = maxIntervalIndependentSet ret prev xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
