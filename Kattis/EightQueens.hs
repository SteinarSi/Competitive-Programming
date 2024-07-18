{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((***), (>>>))
import           Data.Array.Unboxed    (UArray, assocs, bounds, inRange,
                                        listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isControl)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = C.getContents >>= (
            C.filter (isControl >>> not)
        >>> C.unpack
        >>> listArray ((1,1),(8,8))
        >>> solve
        >>> bool "invalid" "valid"
        >>> C.putStrLn
    )

solve :: UArray (Int,Int) Char -> Bool
solve board = length queens == 8 && all safe queens
    where
        queens = assocs board
            & filter (snd >>> (=='*'))
            & map fst

        safe :: (Int,Int) -> Bool
        safe u = [(1,0),(1,1),(0,1),(-1,1)]
            & concatMap (\(dx,dy) -> iterate ((dx+) *** (dy+)) u
                & tail
                & takeWhile (inRange (bounds board))
            )
            & all ((board !) >>> (=='.'))
