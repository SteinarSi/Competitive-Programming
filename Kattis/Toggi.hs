import           Data.Functor ((<&>))

main :: IO ()
main = do
    s <- getLine <&> read

    print $ numeric 20 s (s+1)

numeric :: Int -> Double -> Double -> Int
numeric 0 _ n = floor n
numeric i s n = numeric (i-1) s ((10^6 * s) / logBase 10 n)
