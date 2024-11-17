{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (find, permutations)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> permutations
        >>> find valid
        >>> maybe "Neibb" (map (show >>> C.pack) >>> C.unwords)
        >>> C.putStrLn
    )

valid :: [Int] -> Bool
valid xs = and $ zipWith (gcd >>> (>>> (>1))) xs (drop 1 xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
