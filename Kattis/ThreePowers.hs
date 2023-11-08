import Control.Monad (when)

main :: IO ()
main = do
    n <- fmap (pred . read) getLine
    when (n /= -1) $ do
        putStrLn $ "{ " ++ join (map show (solve powers n)) ++ " }"
        main

solve :: [Integer] -> Integer -> [Integer]
solve [] _ = []
solve _ 0 = []
solve (p:ps) x | x `mod` 2 > 0 = p : solve ps (x `div` 2)
               | otherwise      =     solve ps (x `div` 2)

join :: [String] -> String
join [] = ""
join [x] = x
join (x:y:xs) = x ++ ", " ++ join (y:xs)

powers :: [Integer]
powers = map (3^) [0..]
