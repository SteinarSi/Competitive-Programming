{-# LANGUAGE Strict #-}

import Data.List.Split (splitWhen)
import Data.Bool (bool)
import Prelude hiding (round)
import Data.Function ((&))
import Data.List (sort)

main :: IO ()
main = do
    inn <- fmap (map (map words) . splitWhen null . lines) (readFile "inputs/day11-input.txt")
    let (ms1, is) = parse [] [] True  inn
        (ms2, _)  = parse [] [] False inn
    print (simulate 20 is ms1 , simulate 10000 is ms2)

data Monkey = Monkey {
        name :: Int,
        operation :: Integer -> Integer,
        test :: Integer -> Bool,
        divisor :: Integer,
        friend1 :: Int,
        friend2 :: Int
    }

simulate :: Int -> [[Integer]] -> [Monkey] -> Integer
simulate n items ms = product $ take 2 $ reverse $ sort bs
    where (bs, _) = foldl (&) (replicate (length ms) 0,items) (replicate n (\is -> round (product (map divisor ms)) is ms))

round :: Integer -> ([Integer], [[Integer]]) -> [Monkey] -> ([Integer], [[Integer]])
round f = foldl (\(ns, is) m -> (modify (name m) (+fromIntegral (length (is!!name m))) ns, throw f m (is !! name m) (modify (name m) (const []) is)))

throw :: Integer -> Monkey -> [Integer] -> [[Integer]] -> [[Integer]]
throw f m []     items = items
throw f m (x:xs) items = throw f m xs (modify (bool (friend2 m) (friend1 m) (test m x')) (x':) items)
    where x' = operation m x `mod` f

parse :: [Monkey] -> [[Integer]] -> Bool -> [[[String]]] -> ([Monkey], [[Integer]])
parse ms items dv [] = (reverse ms, reverse items)
parse ms items dv ([[_, n], (_:_:is), (_:_:_:_:op), [_,_,_,d], [_,_,_,_,_,i], [_,_,_,_,_,j]]:xs) = parse (m:ms) (read ('[' : concat is ++ "]") : items) dv xs
    where m = Monkey {
        name = read (init n),
        operation = parseOperation dv op,
        test = (0==) . (`mod` (read d)),
        divisor = read d,
        friend1 = read i,
        friend2 = read j
    }

parseOperation :: Bool -> [String] -> (Integer -> Integer)
parseOperation d [operator, operand] = \a -> dv $ op operator a (bool (read operand) a (operand=="old"))
    where op "*" = (*)
          op "+" = (+)
          dv | d = (`div` 3)
             | otherwise = id

modify :: Int -> (a -> a) -> [a] -> [a]
modify _ _ []     = []
modify 0 f (x:xs) = f x : xs
modify n f (x:xs) = x : modify (n-1) f xs
