module Utils (takeDropWhile, padWith, toArray, mapSome, toTuple, toTriple, read') where

import Data.Array (Array, array)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)


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

toTuple :: Show a => [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple xs = error ("List doesn't have two elements: " ++ show xs)

toTriple :: Show a => [a] -> (a, a, a)
toTriple [a, b, c] = (a, b, c)
toTriple xs = error ("List doesn't have three elements: " ++ show xs)

read' :: (Read r, Show r) => String -> r
read' s = fromMaybe (error ("Could not read " ++ s)) (readMaybe s)
