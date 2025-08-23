{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, bounds, inRange, listArray,
                                        range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isControl)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [h, w, s] <- C.getLine <&> (C.words >>> map readInt)

    graph <- C.getContents <&> (
                C.filter (isControl >>> not)
            >>> C.unpack
            >>> listArray ((1,1),(h,w))
        )

    let Just p1 = find ((graph!) >>> ('@'==)) (range (bounds graph))
        Just pk = find ((graph!) >>> ('$'==)) (range (bounds graph))
        dist = gps graph 0 pk p1 p1 * fromIntegral s

    ["Your destination will arrive in", C.pack (show dist), "meters"]
        & C.unwords
        & C.putStrLn

gps :: UArray (Int,Int) Char -> Integer -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Integer
gps graph d goal prev curr@(x,y)
    | curr == goal = d
    | otherwise = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        & filter (inRange (bounds graph))
        & filter (/= prev)
        & filter ((graph!) >>> (`C.elem` "$."))
        & head
        & gps graph (succ d) goal curr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
