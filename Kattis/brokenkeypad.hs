import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b] <- getLine <&> (words >>> map read)
    print (sevens b - sevens (a-1))

sevens :: Int -> Int
sevens = digits >>> reverse >>> combos

combos :: [Int] -> Int
combos [] = 0
combos (x:xs) = case compare x 7 of
    LT -> x * (10 ^ length xs - 9 ^ length xs) + combos xs
    EQ -> x * (10 ^ length xs - 9 ^ length xs) + decimal xs + 1
    GT -> (x * 10 ^ length xs - (x-1) * 9 ^ length xs) + combos xs

decimal :: [Int] -> Int
decimal = reverse >>> zipWith (*) (map (10^) [0..]) >>> sum

digits :: Int -> [Int]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)
