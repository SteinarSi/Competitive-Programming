{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Data.Array            (Array, assocs, bounds, inRange,
                                        listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (find)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> parse >>> solve)
        >>> C.unlines
        >>> C.putStr
    )

parse :: [C.ByteString] -> Array (Int,Int) C.ByteString
parse xs = listArray ((1,1),(s,s)) xs
  where
    Just s = find ((^2) >>> (==length xs)) [1..]

solve :: Array (Int,Int) C.ByteString -> C.ByteString
solve matrix = assocs matrix
    & find (\((y,x),n) -> all (\ix -> not (inRange (bounds  matrix) ix) || C.length n == C.length (matrix ! ix)) [(y-1,x),(y+1,x),(y,x-1),(y,x+1)])
    & maybe "Name Not Found" (fst >>> (matrix!))
