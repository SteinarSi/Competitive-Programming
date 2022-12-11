{-# LANGUAGE Strict #-}

import Data.List.Split (splitWhen)
import Data.Bool (bool)
import Prelude hiding (round)
import Data.Function ((&))
import Data.List (sort)



main :: IO ()
main = do
    inn <- fmap (map (map words) . splitWhen null . lines) (readFile "day11-input.txt")
    let 
        (ms1, is) = parse [] [] True  inn
        (ms2, _)  = parse [] [] False inn
    print (simulate 20 is ms1) -- , simulate 10000 is ms2)


data Monkey = Monkey {
        name :: Int,
        operation :: Integer -> Integer,
        test :: Integer -> Bool,
        friend1 :: Int,
        friend2 :: Int
    }

instance Show Monkey where
    show m = '\n' : show (name m) ++ "\n" ++ show (test m 20) ++ "\n" ++ show (friend1 m) ++ "," ++ show (friend2 m) ++ "\n"

simulate :: Int -> [[Integer]] -> [Monkey] -> Integer
simulate n items ms = product $ take 2 $ reverse $ sort bs
    where (bs, _) = foldl (&) (replicate (length ms) 0,items) (replicate n (`round` ms))

round :: ([Integer], [[Integer]]) -> [Monkey] -> ([Integer], [[Integer]])
round = foldl (\(ns, is) m -> (modify (name m) (+fromIntegral (length (is!!name m))) ns, throw m (is !! name m) (modify (name m) (const []) is)))

throw :: Monkey -> [Integer] -> [[Integer]] -> [[Integer]]
throw m []     items = items
throw m (x:xs) items = throw m xs (modify (bool (friend2 m) (friend1 m) (test m x')) (x':) items)
    where x' = operation m x

parse :: [Monkey] -> [[Integer]] -> Bool -> [[[String]]] -> ([Monkey], [[Integer]])
parse ms items dv [] = (reverse ms, reverse items)
parse ms items dv ([[monkey, n], 
        (starting:items':is), 
        (operation:new:equals:old:op), 
        [test,divisible,by,d], 
        [iff,true,throw,to,monkey',i], 
        [iff',false,throw',to',monkey'',j]
       ]:xs) = parse (m:ms) (read ('[' : concat is ++ "]") : items) dv xs
    where m = Monkey {
        name = read (init n),
        operation = parseOperation dv op,
        test = (0==) . (`mod` (read d)),
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