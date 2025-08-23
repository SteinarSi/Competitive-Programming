main :: IO ()
main = do
    xss <- fmap (concatMap (\(u, xs) -> format u 1 (words xs)) . zip [1..] . tail . lines) getContents
    print (length xss)
    mapM_ putStrLn xss

format :: Int -> Int -> [String] -> [String]
format _ _ [] = []
format u v ("-1":xs) = format u (succ v) xs
format u v (x:xs) = unwords [show u, show v, x] : format u (succ v) xs
