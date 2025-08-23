{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> filter (\xs -> "+39" `C.isPrefixOf` xs && C.length xs >= 12 && C.length xs <= 13)
        >>> length
        >>> print
    )
