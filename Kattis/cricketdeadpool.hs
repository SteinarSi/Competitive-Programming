import           Control.Arrow ((>>>))
import           Data.Array    (Array, listArray, (!))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,k] <- getLine <&> (words >>> map read)

    let dp = listArray (0,n) (map f [0..n])
        f r | r <= 1    = 1
            | otherwise = [1..min k r]
                & map (\e -> modulo ((choose r e * dp ! (r-e))))
                & sum
                & modulo

    print (dp ! n)

choose :: Integer -> Integer -> Integer
choose n k
    | k > n           = 0
    | k == 0          = 1
    | k > (n `div` 2) = n `choose` (n-k)
    | otherwise       = n * ((n-1) `choose` (k-1)) `div` k

modulo :: Integer -> Integer
modulo = (`mod` 1000000007)
