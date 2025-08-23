{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (\xs -> bool "Chikapu" "Bash" ((C.head xs == C.last xs) == odd (C.length xs)))
        >>> C.unlines
        >>> C.putStr
    )
