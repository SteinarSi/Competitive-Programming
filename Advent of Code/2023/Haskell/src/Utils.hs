module Utils (
    takeDropWhile, padWith, toArray, arrayToString, inBounds, mapSome, tupleToList, toTuple, toTriple,
    read', length', trace', revSort, (!!!), (+++), directions, count', modifyArray, groupOn, ifM,
    spfa, replace, bruh, Cost(..), infinum, supremum, hex, internalPolygonArea, shoelace, extractNaturals,
    extractIntegers, inbetween, hashString, imap
    ) where

import           Control.Applicative (liftA2)
import           Control.Monad       (filterM, forM, forM_)
import           Control.Monad.ST    (ST, runST)
import           Data.Array          (Array, Ix (inRange), array, bounds, (!))
import           Data.Array.MArray   (MArray, newArray, readArray, writeArray)
import           Data.Array.ST       (STArray)
import           Data.Bifunctor      (first)
import           Data.Char           (isDigit, ord)
import           Data.List           (groupBy, sortBy)
import           Data.List.Split     (splitWhen)
import           Data.Maybe          (fromMaybe)
import           Data.Ord            (Down (Down), comparing)
import           Data.Sequence       (Seq (..), (><))
import qualified Data.Sequence       as Seq
import           Debug.Trace         (trace)
import qualified Numeric             as N
import           Text.Read           (readMaybe)


takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile _ [] = ([], [])
takeDropWhile p (x:xs) | p x       = first (x:) (takeDropWhile p xs)
                       | otherwise = ([], x:xs)

padWith :: a -> [[a]] -> [[a]]
padWith a xss = row : [a : xs ++ [a] | xs <- xss] ++ [row]
    where row = replicate (length (head xss) + 2) a

toArray :: [[a]] -> Array (Int, Int) a
toArray xss = array b l
    where l = [ ((x, y), s) | (y, xs) <- zip [0..] xss, (x, s) <- zip [0..] xs]
          b = ((0, 0), (length (head xss)-1, length xss-1))

arrayToString :: (Ix i, Enum i) => Array (i, i) a -> ((i, i) -> a -> String) -> String
arrayToString arr f = unlines [ row y | y <- [y1..y2] ]
    where row y = concat [ f (x, y) (arr ! (x, y)) | x <- [x1..x2] ]
          ((x1, y1), (x2, y2)) = bounds arr

extractNaturals :: (Num a, Read a) => String -> [a]
extractNaturals = extractIntegers . filter (/='-')

extractIntegers :: (Num a, Read a) => String -> [a]
extractIntegers = map read' . filter (not . null) . splitWhen (\c -> not (c == '-' || isDigit c))

inBounds :: (Ix i) => Array i a -> i -> Bool
inBounds arr = inRange (bounds arr)

inbetween :: Ord a => a -> (a, a) -> Bool
inbetween x (lower, upper) = lower <= x && x <= upper

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

-- | Given a polygon as a list of coordinates, determine the internal area of the polygon.
-- Remember that the list must loop: head xs == last xs
internalPolygonArea :: [(Int, Int)] -> Int
internalPolygonArea path = shoelace path - len `div` 2 + 1
    where len = sum $ zipWith manhattan path (tail path)

-- | Given a polygon given by coordinates, determine the area of the polygon.
-- Remember that the list must loop: head xs == last xs
shoelace :: [(Int, Int)] -> Int
shoelace xs = abs (sum (zipWith (\(x1,y1) (x2,y2) -> (y1+y2) * (x1-x2)) xs (tail xs))) `div` 2

hashString :: Int -> Int -> String -> Int
hashString a b = hashString' 0
    where
        hashString' :: Int -> String -> Int
        hashString' h ""     = h
        hashString' h (x:xs) = hashString' (((h + ord x) * a) `mod` b) xs

manhattan :: Num a => (a,a) -> (a,a) -> a
manhattan (x,y) (a,b) = abs (x-a) + abs (y-b)

revSort :: Ord a => [a] -> [a]
revSort = sortBy (comparing Down)

read' :: Read r => String -> r
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

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0..]

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr i f = readArray arr i >>= writeArray arr i . f

infinum :: (Foldable f, Ord a, Bounded a) => f a -> a
infinum = foldr min maxBound

supremum :: (Foldable f, Ord a, Bounded a) => f a -> a
supremum = foldr max minBound

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

replace :: Int -> [a] -> a -> [a]
replace 0 (_:xs) a = a:xs
replace i (x:xs) a = x : replace (i-1) xs a
replace _ []     _ = []

bruh :: Show a => a -> b
bruh a = error ("bruh: " ++ show a)

hex :: String -> Int
hex = fst . head . N.readHex

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond left right = do
    b <- cond
    if b
        then left
        else right

data Cost i = Cost { cost :: i } | Inf
    deriving (Eq, Ord, Functor)

instance Applicative Cost where
    pure = Cost
    (<*>) (Cost f) (Cost x) = Cost (f x)
    (<*>) _ _               = Inf

instance Num a => Num (Cost a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    fromInteger = Cost . fromInteger
    abs = fmap abs
    signum = fmap signum

instance Num a => Bounded (Cost a) where
    minBound = Cost 0
    maxBound = Inf

-- TODO why is this so slow?
spfa :: forall i n. (Ix i, Ord n, Num n) => (i -> [(n, i)]) -> (i, i) -> [i] -> [i] -> Cost n
spfa neighbourhood range start stop = runST $ do
    dist <- newArray range Inf
    seen <- newArray range False
    forM_ start $ \s -> do
        writeArray dist s 0
        writeArray seen s True
    spfa' seen dist (Seq.fromList start)

    where
        spfa' :: STArray s i Bool -> STArray s i (Cost n) -> Seq i -> ST s (Cost n)
        spfa' _ dist Empty = minimum <$> forM stop (readArray dist)
        spfa' seen dist (u :<| queue) = do
            distu <- readArray dist u
            next <- filterM (\(c, v) -> do
                distv <- readArray dist v
                if distu + Cost c < distv
                    then do
                        writeArray dist v (distu + Cost c)
                        not <$> readArray seen v
                    else pure False
                ) (neighbourhood u)
            spfa' seen dist (queue >< Seq.fromList (map snd next))








