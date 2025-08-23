{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map (C.filter isDigit
            >>> readInt
            >>> (`mod` 3)
            >>> (/=2)
            >>> bool "IMPOSSIBLE" "VALID")
        >>> C.unlines
        >>> C.putStr
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
