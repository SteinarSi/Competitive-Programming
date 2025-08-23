{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (freeze, newArray_, writeArray, (!))
import           Data.Array.ST         (STUArray)
import           Data.Array.Unboxed    (UArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    ts <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    C.putStrLn (solve n ts)

solve :: Int -> [[Int]] -> C.ByteString
solve n ts | not associative = "magma"
           | otherwise = case identity of
                Nothing -> "semigroup"
                Just  i | inverse i -> "group"
                        | otherwise -> "monoid"
    where
        xs :: [Int]
        xs = [0..n-1]

        (•) :: (Int -> Int -> Int)
        (•) = runST $ do
            arr <- newArray_ ((0,0),(n-1,n-1))
            forM_ ts $ \(a:b:c:_) -> writeArray arr (a,b) c
            f <- (freeze :: STUArray s (Int,Int) Int -> ST s (UArray (Int,Int) Int)) arr
            pure (curry (f!))

        associative :: Bool
        associative = and $ do
            x <- xs
            y <- xs
            z <- xs
            pure ((x • y) • z == x • (y • z))

        identity :: Maybe Int
        identity = find (\i -> all (\x -> x • i == x && i • x == x) xs) xs

        inverse :: Int -> Bool
        inverse i = all (\x -> any (\y -> x • y == i && y • x == i) xs) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
