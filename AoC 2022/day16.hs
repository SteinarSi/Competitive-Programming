{-# LANGUAGE TupleSections #-}

import Data.Int (Int8)

import Data.Array (Array, Ix, array, (!), assocs, bounds, indices)
import qualified Data.Set as S
import Data.List (sort, find)
import Data.Bifunctor (bimap, first, second)
import Data.Char (ord)
import Data.Maybe (fromJust)

import Debug.Trace (trace)

main :: IO ()
main = do
    (g,f) <- fmap preprocess (readFile "day16-input.txt")
    print (brute g f)


type Flow  = Array Int8 Int
type Graph = Array Int8 [(Int8, Int8)]

brute = bruteForce 0 30 0 S.empty S.empty

bruteForce :: Int8 -> Int8 -> Int -> S.Set Int8 -> S.Set Int8 -> Graph -> Flow -> Int
bruteForce i time current turned visited g f | time <= 0 = 0
                                     | S.member i turned && null neighbourCalls = fromIntegral time * current
                                     | S.member i turned = maximum neighbourCalls
                                     | otherwise = maximum (current + bruteForce i (time-1) (current+ f ! i) (S.insert i turned) S.empty g f : neighbourCalls)
    where 
        neighbourCalls :: [Int]
        neighbourCalls = map (\(w,n) -> fromIntegral w * current + bruteForce n (time-w) current turned (S.insert n visited) g f) neighbours

        neighbours :: [(Int8, Int8)]
        neighbours = filter (\(w,n) -> w <= time && S.notMember n visited) (g ! i)

preprocess :: String -> (Graph, Flow)
preprocess s = (contract (indices g) g f, f)
    where (g,f) = parse S.empty [] [] $ map words $ lines s

parse :: S.Set String -> [(String, [(Int8, String)])] -> [(String, Int)] -> [[String]] -> (Graph, Flow)
parse names neighbours flows [] = let aix = array (0, fromIntegral (S.size names)-1)
                                      translate = fromJust . flip lookup (zip (sort $ S.toList names) [0..])
                                      neighbours' = map (bimap translate (map (second translate))) neighbours
                                      flows' = map (first translate) flows
                                  in  (aix neighbours', aix flows')
parse names neighbours flows ((_:name:_:_:flow:_:_:_:_:neighs):xs) = parse (S.insert name names) ((name,ns):neighbours) ((name,f):flows) xs
    where ns = map (1,) $ last neighs : map init (init neighs)
          f  = read (init (drop 5 flow))

contract :: [Int8] -> Graph -> Flow -> Graph
contract [] g _ = g
contract (x:xs) g f | f ! x /= 0 = contract xs g f
                    | otherwise  = contract xs (imap combine g) f
    where combine :: Int8 -> [(Int8, Int8)] -> [(Int8, Int8)]
          combine y ns = case fmap fst (find ((==x) . snd) ns) of
            Just d  -> foldr insert (map (first (d+)) $ filter ((/=y) . snd) (g ! x)) (filter ((/=x) . snd) ns)
            Nothing | y == x && x /= 0 -> []
                    | otherwise        -> ns
          insert :: (Ord a, Eq b) => (a,b) -> [(a,b)] -> [(a,b)]
          insert a [] = [a]
          insert (a,b) ((a',b'):xs) | b == b'   = (min a a', b) : xs
                                    | otherwise = (a',b') : insert (a,b) xs

imap :: Ix i => (i -> a -> b) -> Array i a -> Array i b
imap f arr = array (bounds arr) [ (i, f i a) | (i, a) <- assocs arr ]

