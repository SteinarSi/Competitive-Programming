import           Data.Functor ((<&>))
import           Data.List    (foldl1')

m :: Integer
m = 1000000007

main :: IO ()
main = do
    n <- getLine <&> read

    print $ foldl1' mu [
            n,
            n-1,
            n-2,
            n-3,
            n-4,
            n-5,
            n-6,
            ((n-7) * (n-8)) `di` 2,
            ((n-9) * (n-10)) `di` 2,
            n-11,
            n-12,
            n-13,
            n-14,
            n-15,
            n-16,
            ((n-17) * (n-18)) `di` 2,
            n-19
        ]

di :: Integer -> Integer -> Integer
di a b = (a `div` b) `mod` m

mu :: Integer -> Integer -> Integer
mu a b = (a * b) `mod` m
