{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    [n,h,w] <- C.getLine <&> (C.words >>> map readInt)

    images <- C.getContents <&> C.filter (`C.elem` ".*")

    [0..h*w-1]
        & map (\i -> [i,i+h*w .. h*w*n-1]
                & map (C.index images )
                & C.pack)
        & S.fromList
        & S.size
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
