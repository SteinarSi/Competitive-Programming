{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>), (***))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (elemIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main =  C.getContents >>= (
            C.words 
        >>> elemIndex "1" 
        >>> fromJust 
        >>> (`quotRem` 5) 
        >>> (subtract 2 >>> abs) 
                *** 
            (subtract 2 >>> abs) 
        >>> uncurry (+) 
        >>> print
    )
