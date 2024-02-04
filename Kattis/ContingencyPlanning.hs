import           Control.Arrow ((>>>))
import           Data.Array    (Array, array, (!))

main :: IO ()
main = getLine >>= (
            read
        >>> zombies 1
        >>> fightOrFlight 0
        >>> putStrLn
    )

fightOrFlight :: Int -> [Int] -> String
fightOrFlight curr [] = show curr
fightOrFlight curr (x:xs) | x + curr > 10^9 = "JUST RUN!!"
                          | otherwise       = fightOrFlight (x + curr) xs

zombies :: Int -> Int -> [Int]
zombies k n | k > n     = []
            | otherwise = fac ! k * (n `choose` k) : zombies (k+1) n

choose :: Int -> Int -> Int
choose n k | k > n           = 0
           | k == 0          = 1
           | k > (n `div` 2) = n `choose` (n-k)
           | otherwise       = n * ((n-1) `choose` (k-1)) `div` k

fac :: Array Int Int
fac = array (0, 100) $ (0, 1) : [ (i, i * fac ! (i-1)) | i <- [1..100] ]
