{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           System.IO             (hFlush, stdout)

main :: IO ()
main = do
    n <- C.getLine <&> readInt

    [1..n]
        & map (show >>> C.pack)
        & ("?":)
        & C.unwords
        & C.putStrLn
    hFlush stdout

    r <- C.getLine <&> readInt
    C.putStrLn ("! " <> C.pack (show (r - (n * n * (n+1)) `div` 2)))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
