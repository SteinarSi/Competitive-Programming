{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> head &&& (last >>> readInt))
        >>> teque (Seq.empty,Seq.empty)
        >>> map show
        >>> unlines
        >>> putStr
    )

teque :: (Seq Int, Seq Int) -> [(C.ByteString,Int)] -> [Int]
teque _ [] = []
teque (xs,ys) ((o,x):qs) = case o of
        "push_front"  -> teque (balance (x :<| xs, ys)) qs
        "push_back"   -> teque (balance (xs, ys :|> x)) qs
        "push_middle" -> teque (balance (xs, x :<| ys)) qs
        "get" | x >= Seq.length xs -> Seq.index ys (x - Seq.length xs) : teque (xs,ys) qs
              | otherwise          -> Seq.index xs x                   : teque (xs,ys) qs

balance :: (Seq Int, Seq Int) -> (Seq Int, Seq Int)
balance (xs,ys) | m > n     = left (xs,ys)
                | n > m+1   = right (xs,ys)
                | otherwise = (xs,ys)
  where
    n = Seq.length xs
    m = Seq.length ys

right :: (Seq Int, Seq Int) -> (Seq Int, Seq Int)
right (xs :|> x,ys) = (xs, x :<| ys)

left :: (Seq Int, Seq Int) -> (Seq Int, Seq Int)
left (xs, y :<| ys) = (xs :|> y, ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
