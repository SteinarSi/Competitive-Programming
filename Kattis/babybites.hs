{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         (second, (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> zip [1..]
        >>> filter (snd >>> (/="mumble"))
        >>> map (second readInt)
        >>> filter (uncurry (/=))
        >>> null
        >>> bool "something is fishy" "makes sense"
        >>> C.putStrLn
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
