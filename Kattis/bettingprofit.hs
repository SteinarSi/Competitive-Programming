{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map C.words
        >>> solve
        >>> map show
        >>> unlines
        >>> putStr
    )

solve :: [[C.ByteString]] -> [Double]
solve []             = []
solve ([m,o,"+"]:xs) = readNum o * (readNum m / 100) : solve xs
solve ([m,o,"-"]:xs) = readNum m * 100 / readNum o : solve xs

readNum :: Num n => C.ByteString -> n
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
