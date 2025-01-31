{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> map (readInt
            >>> iterate (`div` 7)
            >>> takeWhile (>0)
            >>> map ((`mod` 7)
            >>> (options!))
            >>> C.concat)
        >>> C.unlines
        >>> C.putStr
    )

options :: Array Int C.ByteString
options = listArray (0,6) ["0","1","2","5","9","8","6"]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
