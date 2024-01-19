main :: IO ()
main = do
    [fizz, buzz, n] <- fmap (map read . words) getLine
    mapM_ (putStrLn . fizzBuzz fizz buzz) [1..n]

fizzBuzz :: Int -> Int -> Int -> String
fizzBuzz fizz buzz i = case (i `mod` fizz, i `mod` buzz) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    (_, _) -> show i
