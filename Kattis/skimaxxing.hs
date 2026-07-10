{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (elemIndex, find, sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = do
    kmy:rest <- C.getContents <&> C.lines
    let [k,m,y] = map readInt (C.words kmy)
        (mnts,(fave,qs)) = splitAt m rest
            & (map (C.words >>> \[n,a,d] -> (fromJust (elemIndex d difficulties), readInt a, n)) >>> sortOn Down)
                ***
              (head &&& (drop 1 >>> map readInt))
        Just (_,fa,_) = find (\(_,_,n) -> n == fave) mnts

        solve :: (Int,Int) -> [Int] -> [C.ByteString]
        solve _ []           = []
        solve (lo,hi) (x:xs)
            | x > fa || x <= lo = "NO" : solve (lo,hi) xs
            | x >= hi = "YES" : solve (lo,hi) xs
            | possible = "YES" : solve (lo,x) xs
            | otherwise = "NO" : solve (x,hi) xs
          where
            possible = any (\(_,_,n) -> n == fave) (take k (filter (\(_,a,_) -> a >= x) mnts))

    solve (minBound,maxBound) qs
        & C.unlines
        & C.putStr

difficulties :: [C.ByteString]
difficulties = ["Green", "Blue", "Red", "Black", "DoubleBlack"]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
