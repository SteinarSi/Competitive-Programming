module Utils (takeDropWhile, padWith, toArray, mapSome) where

import Data.Bifunctor (first)
import Data.Array (Array, array)

takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile _ [] = ([], [])
takeDropWhile p (x:xs) | p x       = first (x:) (takeDropWhile p xs)
                       | otherwise = ([], x:xs)

padWith :: a -> [[a]] -> [[a]]
padWith a xss = row : [a : xs ++ [a] | xs <- xss] ++ [row]
    where row = replicate (length (head xss) + 2) a

toArray :: [String] -> Array (Int, Int) Char
toArray xss = array b l
    where l = [ ((x, y), s) | (y, xs) <- zip [0..] xss, (x, s) <- zip [0..] xs]
          b = ((0, 0), (length (head xss)-1, length xss-1))

mapSome :: Integral i => (a -> a) -> i -> [a] -> [a]
mapSome _ 0 xs = xs
mapSome _ _ [] = []
mapSome f i (x:xs) = f x : mapSome f (pred i) xs
