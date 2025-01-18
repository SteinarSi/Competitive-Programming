{-# LANGUAGE BangPatterns #-}

import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    l:r:k:xs <- C.getContents <&> (C.words >>> map readInt)
    print (counter (l-1) r 0 (combinations r (0,1) xs))

counter :: Int -> Int -> Int -> [(Int,Int)] -> Int
counter l r !ret [] = ret
counter l r !ret ((c,p):xs) = counter l r (ret + bool (-1) 1 (odd c) * (r `div` p - l `div` p)) xs

combinations :: Int -> (Int,Int) -> [Int] -> [(Int,Int)]
combinations r (0,_) [] = []
combinations r (c,p) [] = [(c,p)]
combinations r (c,p) (x:xs) | p * x > r = skip
                            | otherwise = pick <> skip
    where skip = combinations r (c,p) xs
          pick = combinations r (c+1,p*x) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
