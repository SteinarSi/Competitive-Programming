import           Prelude hiding (gcd)

main :: IO ()
main = do
    [a, b] <- fmap (map read . words) getLine
    print (gcd a b)

gcd :: Integral i => i -> i -> i
gcd a 0 = a
gcd a b = gcd b (a `mod` b)
