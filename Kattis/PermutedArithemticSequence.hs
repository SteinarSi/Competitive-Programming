{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> tail >>> map readInt >>> solve)
        >>> mapM_ C.putStrLn
    )

solve :: [Int] -> C.ByteString
solve xs | arithmetic xs        = "arithmetic"
         | arithmetic (sort xs) = "permuted arithmetic"
         | otherwise            = "non-arithmetic"

arithmetic :: [Int] -> Bool
arithmetic l@(x:y:xs) = and (zipWith (==) [x,y..] l)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
