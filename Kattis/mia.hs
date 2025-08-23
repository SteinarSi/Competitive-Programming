{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.List             (elemIndex)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words
            >>> map readInt
            >>> (\[a,b,c,d] -> ((max a b, min a b), (max c d, min c d)))
            >>> solve
            )
        >>> mapM_ C.putStrLn
    )

solve :: ((Int,Int), (Int,Int)) -> C.ByteString
solve (s, r) = case on compare ((`elemIndex` rolls) >>> fromJust) s r of
    LT -> "Player 1 wins."
    GT -> "Player 2 wins."
    EQ -> "Tie."

rolls :: [(Int,Int)]
rolls = concat [
        [(2,1)],
        [ (i,i) | i <- [6,5..1] ],
        [ (i,j) | i <- [6,5..1], j <- [i-1,i-2..1] ]
    ]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
