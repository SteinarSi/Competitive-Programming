{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.IntSet           as S

main :: IO ()
main = do
    [n,c,s,f] <- C.getLine <&> (C.words >>> map readInt)
    [n, c, s, n-f]
        & map (show >>> C.pack)
        & C.unwords
        & C.putStrLn
    C.getLine >>= C.putStrLn
    fs <- C.getLine <&> (C.words >>> map readInt >>> S.fromList)
    [1..n]
        & filter (`S.notMember` fs)
        & map (show >>> C.pack)
        & C.unwords
        & C.putStrLn
    C.interact id

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
