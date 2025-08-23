main :: IO ()
main = solve 210 <$> fmap read getLine <*> fmap (map ((\(t:q:_) -> (read t, head q)) . words) . tail . lines) getContents >>= print

solve :: Int -> Int -> [(Int, Char)] -> Int
solve t k [] = error "bruh, the description is a lie"
solve t k ((x,a):xs) | t - x < 0 = k
                     | a == 'T' = solve (t - x) (k `mod` 8 + 1) xs
                     | otherwise = solve (t - x) k xs
