{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
                C.lines
            >>> filter (/= "0 0 0")
            >>> map (C.words
                >>> map readInt
                >>> (\[a,b,c] -> bool "wrong" "right" ((a^2 + b^2 == c^2) || (a^2 == b^2 + c^2) || (a^2 + c^2 == b^2))))
            >>> mapM_ C.putStrLn
        )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
