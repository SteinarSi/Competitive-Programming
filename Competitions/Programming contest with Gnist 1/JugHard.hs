{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.words
                >>> map readInt
                >>> (\(a:b:d:_) -> d `mod` gcd a b == 0)
                >>> bool "No" "Yes"
                >>> C.putStrLn
            )
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
