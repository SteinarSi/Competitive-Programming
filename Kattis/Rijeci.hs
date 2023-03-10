import Data.List
import Data.Bits

main :: IO()
main = do
    input <- getLine
    let tall = read input::Int
    putStr (show (fib (tall-1)) ++ " " ++ show (fib tall))

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n-1)) + (fibonacci (n-2))

fib :: Int -> Integer
fib n = snd . foldl_ fib_ (1, 0) . dropWhile not $ [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib_ (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g
        foldl_ = foldl' -- '