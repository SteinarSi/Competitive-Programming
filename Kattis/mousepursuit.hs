{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (foldl')
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    (events,k) <- C.getContents <&> (C.lines
            >>> tail
            >>> (init >>> map (C.words >>> head &&& (tail >>> map readInt) >>> parse))
                    &&&
                (last >>> readInt)
        )
    filter (fst >>> (<=k)) events
            & foldl' (\(cs,gs) (_,(c,g)) -> (cs+c,gs+g)) (0,0)
            & uncurry (printf "%d %d\n")

parse :: (C.ByteString, [Int]) -> (Int,(Int,Int))
parse ("CAUGHT",[s,c,g]) = (s, ( c, g))
parse ("MISS"  ,[s,c,g]) = (s, (-c,-g))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
