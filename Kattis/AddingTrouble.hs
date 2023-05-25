import Data.Bool (bool)

main :: IO ()
main = fmap (map read . words) getLine >>= \(a:b:c:_) -> putStrLn $ bool "wrong!" "correct!" (a+b == c)