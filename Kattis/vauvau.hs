main :: IO ()
main = ((getLine >>=) . (. (map read . words)) . mapM_ . (putStrLn .) . dogs) . map read . words =<< getLine

dogs :: [Integer] -> Integer -> String
dogs [a, b, c, d] t | dog a b && dog c d = "both"
                    | dog a b || dog c d = "one"
                    | otherwise = "none"
    where dog x y = (t-1) `mod` (x+y) < x
