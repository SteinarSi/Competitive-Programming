{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    c:_:errors <- fmap (C.words >>> map readInt) C.getContents
    let (correct, wrong) = parse c errors
    C.putStrLn ("Errors: " <> format wrong)
    C.putStrLn ("Correct: " <> format correct)

format :: [(Int,Int)] -> C.ByteString
format xs = case map formatRange xs of
    []     -> error "bruh"
    [x]    -> x
    (x:xs) -> C.intercalate ", " (reverse xs) <> " and " <> x
    where
        formatRange :: (Int,Int) -> C.ByteString
        formatRange (x,y) | x == y    = C.pack (show x)
                          | otherwise = C.pack (show x) <> "-" <> C.pack (show y)

parse :: Int -> [Int] -> ([(Int,Int)], [(Int,Int)])
parse c (x:xs) | x == 1    = parse c x x []        [] xs
               | otherwise = parse c x x [(1,x-1)] [] xs
    where
        parse :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)] -> [Int] -> ([(Int,Int)], [(Int,Int)])
        parse c start end correct wrong [] | end == c = (correct, (start,end) : wrong)
                                           | otherwise = ((end+1, c) : correct, (start, end) : wrong)
        parse c start end correct wrong (x:xs) | x == end + 1 = parse c start x correct wrong xs
                                               | otherwise = parse c x x ((end+1,x-1) : correct) ((start,end):wrong) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
