module Utils (
    takeDropWhile, padWith, toArray, arrayToString,
    inBounds, mapSome, tupleToList, toTuple, toTriple,
    read', length', trace', revSort, (!!!), (+++),
    directions, count', modifyArray, groupOn
    ) where

import           Data.Array        (Array, Ix (inRange), array, bounds, (!))
import           Data.Array.MArray (MArray, readArray, writeArray)
import           Data.Bifunctor    (first)
import           Data.List         (groupBy, sortBy)
import           Data.Maybe        (fromMaybe)
import           Data.Ord          (Down (Down), comparing)
import           Debug.Trace       (trace)
import           Text.Read         (readMaybe)


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

arrayToString :: (Ix i, Enum i) => Array (i, i) a -> ((i, i) -> a -> String) -> String
arrayToString arr f = unlines [ row y | y <- [y1..y2] ]
    where row y = concat [ f (x, y) (arr ! (x, y)) | x <- [x1..x2] ]
          ((x1, y1), (x2, y2)) = bounds arr

inBounds :: (Ix i) => Array i a -> i -> Bool
inBounds arr = inRange (bounds arr)

mapSome :: Integral i => (a -> a) -> i -> [a] -> [a]
mapSome _ 0 xs     = xs
mapSome _ _ []     = []
mapSome f i (x:xs) = f x : mapSome f (pred i) xs

tupleToList :: (a, a) -> [a]
tupleToList (a, b) = [a, b]

toTuple :: Show a => [a] -> (a, a)
toTuple [a, b] = (a, b)
toTuple xs     = error ("List doesn't have two elements: " ++ show xs)

toTriple :: Show a => [a] -> (a, a, a)
toTriple [a, b, c] = (a, b, c)
toTriple xs        = error ("List doesn't have three elements: " ++ show xs)

revSort :: Ord a => [a] -> [a]
revSort = sortBy (comparing Down)

read' :: (Read r, Show r) => String -> r
read' s = fromMaybe (error ("Could not read " ++ s)) (readMaybe s)

length' :: Integral i => [a] -> i
length' = fromIntegral . length

trace' :: Show s => s -> a -> a
trace' s = trace (show s)

(!!!) :: (Ix i, Show i) => Array i a -> i -> a
(!!!) arr i | inBounds arr i = arr ! i
            | otherwise      = error ("Error in array index: " ++ show i ++ " is not in " ++ show (bounds arr))

(+++) :: Num a => (a, a) -> (a, a) -> (a, a)
(+++) (a, b) (x, y) = (a+x, b+y)

directions :: [(Int, Int)]
directions = [(0,1), (0,-1), (1,0), (-1,0)]

count' :: Eq a => a -> [a] -> Int
count' _ [] = 0
count' a (x:xs) | a == x = 1 + count' a xs
               | otherwise = count' a xs


modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr i f = readArray arr i >>= writeArray arr i . f

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)
