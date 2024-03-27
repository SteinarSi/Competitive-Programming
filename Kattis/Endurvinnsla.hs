{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    C.getLine
    p <- getLine <&> read
    n <- C.getLine <&> (C.readInt >>> fromJust >>> fst >>> fromIntegral)
    m <- C.getContents <&> (C.lines >>> filter ("plast"/=) >>> length >>> fromIntegral)
    C.putStrLn $ if m / n <= p
        then "Jebb"
        else "Neibb"
