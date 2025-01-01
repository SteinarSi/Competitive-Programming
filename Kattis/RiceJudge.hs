{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.List             (maximumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:cmp:xs <- C.getContents <&> C.lines
    xs
        & map (C.words >>> map readInt)
        & zip [1..]
        & maximumBy (compare `on` (snd >>> comparator cmp))
        & fst
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

comparator :: C.ByteString -> [Int] -> (Int,Int)
comparator "antal"   [a,s] = (a+s,a)
comparator "storlek" [a,s] = (a+s,s)
