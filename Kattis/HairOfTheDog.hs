{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (ap)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (=="drunk")
        >>> ap zip tail
        >>> filter (\(a,b) -> a && not b)
        >>> length
        >>> print
    )
