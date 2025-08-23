{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>), (&&&))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (listArray)
import           Data.Array.Base       (STUArray, writeArray, readArray, newArray, bounds, indices, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> head &&& last)
        >>> split
        >>> map solve
        >>> C.unlines
        >>> C.putStr
    )

solve :: (Int,[(Int,Int)]) -> C.ByteString
solve (n,xs) = runST $ do
        seen <- newArray (bounds pos) False
        bfs seen [0]
        readArray seen (n+1) <&> bool "sad" "happy"
    where
        pos = listArray (0,n+1) xs

        bfs :: STUArray s Int Bool -> [Int] -> ST s ()
        bfs seen [] = pure ()
        bfs seen ys = indices pos
                & filter (\x -> any (\y -> manhattan (pos ! x) (pos ! y) <= 50*20) ys)
                & filterM (\x -> readArray seen x >>= \r -> if r then pure False else writeArray seen x True >> pure True)
                >>= bfs seen

split :: [(Int,Int)] -> [(Int,[(Int,Int)])]
split [] = []
split ((n,_):xs) = let (a,b) = splitAt (n+2) xs
                   in  (n,a) : split b

manhattan :: (Int,Int) -> (Int,Int) -> Int
manhattan (a,b) (x,y) = abs (a-x) + abs (b-y)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>>  fst
