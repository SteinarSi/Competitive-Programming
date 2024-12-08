{-# LANGUAGE Strict #-}

module Year.Year2022.Day11 (Day11(Day11)) where

import           Control.Arrow   ((>>>))
import           Data.Bool       (bool)
import           Data.Function   ((&))
import           Data.List       (foldl', sort)
import           Data.List.Split (splitWhen)
import           Prelude  hiding (round)

import           Meta            (AoC (..))

data Day11 = Day11
instance AoC Day11 ([Monkey], [[Integer]]) Integer where
    date _ = (11,2022)
    parse _ = lines >>> splitWhen null >>> map (map words) >>> parser [] []
    part1 _ (ms, is) = simulate 20 is (map div3 ms)
    part2 _ (ms, is) = simulate 10000 is ms
    testAnswerPart1 _ = 10605
    testAnswerPart2 _ = 2713310158

data Monkey = Monkey {
        name :: Int,
        operation :: Integer -> Integer,
        test :: Integer -> Bool,
        divisor :: Integer,
        friend1 :: Int,
        friend2 :: Int
    }

simulate :: Int -> [[Integer]] -> [Monkey] -> Integer
simulate n items ms = replicate n (\is -> round (product (map divisor ms)) is ms)
        & foldl' (&) (replicate (length ms) 0,items)
        & fst
        & sort
        & reverse
        & take 2
        & product

round :: Integer -> ([Integer], [[Integer]]) -> [Monkey] -> ([Integer], [[Integer]])
round f = foldl (\(ns, is) m -> (modify (name m) (+fromIntegral (length (is!!name m))) ns, throw f m (is !! name m) (modify (name m) (const []) is)))

throw :: Integer -> Monkey -> [Integer] -> [[Integer]] -> [[Integer]]
throw f m []     items = items
throw f m (x:xs) items = throw f m xs (modify (bool (friend2 m) (friend1 m) (test m x')) (x':) items)
    where x' = operation m x `mod` f

parser :: [Monkey] -> [[Integer]] -> [[[String]]] -> ([Monkey], [[Integer]])
parser ms items [] = (reverse ms, reverse items)
parser ms items ([[_, n], (_:_:is), (_:_:_:_:op), [_,_,_,d], [_,_,_,_,_,i], [_,_,_,_,_,j]]:xs) = parser (m:ms) (read ('[' : concat is ++ "]") : items) xs
    where m = Monkey {
        name = read (init n),
        operation = parseOperation op,
        test = (0==) . (`mod` (read d)),
        divisor = read d,
        friend1 = read i,
        friend2 = read j
    }

parseOperation :: [String] -> (Integer -> Integer)
parseOperation [operator, operand] = \a -> op operator a (bool (read operand) a (operand=="old"))
    where op "*" = (*)
          op "+" = (+)

modify :: Int -> (a -> a) -> [a] -> [a]
modify _ _ []     = []
modify 0 f (x:xs) = f x : xs
modify n f (x:xs) = x : modify (n-1) f xs

div3 :: Monkey -> Monkey
div3 m = m { operation = operation m >>> (`div` 3) }
