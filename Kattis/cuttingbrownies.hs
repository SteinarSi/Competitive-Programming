{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words
            >>> (\[breadth,depth,player] -> (readInt breadth, readInt depth, player))
            >>> solve
        )
        >>> C.unlines
        >>> C.putStr
    )

solve :: (Int,Int,C.ByteString) -> C.ByteString
solve (b, d, p) | on (>) (fromIntegral >>> logBase 2 >>> floor) x y = p <> " can win"
                | otherwise    = p <> " cannot win"
    where (x,y) | p == "Harry" = (d, b)
                | otherwise    = (b, d)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
