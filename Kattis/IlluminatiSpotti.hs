{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (guard)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> (C.readInt >>> fromJust >>> fst)
    graph <- C.getContents <&> (C.words >>> map ("1"==) >>> listArray ((1,1),(n,n))) :: IO (UArray (Int,Int) Bool)
    print $ length $ do
        i <- [1..n-2]
        j <- [i+1..n-1]
        guard (graph ! (i,j))
        k <- [j+1..n]
        guard (graph ! (i,k))
        guard (graph ! (j,k))
        pure ()
