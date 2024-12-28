{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Bool             (bool)
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import qualified Data.IntSet           as S
import           Data.Ix               (inRange)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt >>> jolly)
        >>> C.unlines
        >>> C.putStr
    )

jolly :: [Int] -> C.ByteString
jolly (n:xs) = zipWith (\x y -> abs (x-y)) xs (drop 1 xs)
        & filter (inRange (1,n-1))
        & S.fromList
        & S.size
        & (==n-1)
        & bool "Not jolly" "Jolly"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
