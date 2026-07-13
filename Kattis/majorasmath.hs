{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    h <- C.getLine <&> readInt
    [a,b,c,d] <- C.getLine <&> (C.words >>> map readInt)
    dmg <- C.getContents <&> (
            C.lines
        >>> map (\case
            "standard" -> a
            "fire" -> b
            "ice" -> c
            "light" -> d)
        >>> sum)
    putStrLn $ if dmg >= h
        then "dead"
        else show (h - dmg)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
