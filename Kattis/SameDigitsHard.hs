{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (guard)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [a, b] <- C.getLine <&> (C.words >>> map readInt)
    let pairs = sameDigits a b
    C.putStrLn (show' (length pairs) <> " digit-preserving pair(s)")
    mapM_ C.putStrLn pairs

sameDigits :: Int -> Int -> [C.ByteString]
sameDigits x b | x > b     = []
               | otherwise = sameDigits' [x..b] <> sameDigits (x+1) b
    where
        sameDigits' :: [Int] -> [C.ByteString]
        sameDigits' [] = []
        sameDigits' (y:ys) | x*y > b = []
                           | sort (digits x ++ digits y) == sort (digits (x*y)) = format x y : sameDigits' ys
                           | otherwise = sameDigits' ys

digits :: Int -> [Int]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

{-# INLINE format #-}
format :: Int -> Int -> C.ByteString
format x y = "x = " <> show' x <> ", y = " <> show' y <> ", xy = " <> show' (x*y)

show' :: Show s => s -> C.ByteString
show' = show >>> C.pack

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
