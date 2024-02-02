main :: IO ()
main = do
    [a, b, c] <- fmap (map read . words) getLine
    let (d, e) = tri (a, b, c)
    putStrLn $ concat [show a, d, show b, e, show c]

tri :: (Int, Int, Int) -> (String, String)
tri (a, b, c) | a + b == c = ("+", "=")
              | a == b + c = ("=", "+")
              | a - b == c = ("-", "=")
              | a == b - c = ("=", "-")
              | a * b == c = ("*", "=")
              | a == b * c = ("=", "*")
              | a `div` b == c = ("/", "=")
              | a == b `div` c = ("=", "/")
              | otherwise = error "bruh"
