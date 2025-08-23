import Data.List (sort)
main = do
    getLine
    (m,i):xs <- fmap (reverse . sort . flip zip [1..] . map read . words) getLine
    if m > sum (map fst xs)
        then putStrLn "impossible"
        else putStr (show i) >> putStrLn (unwords $ map (show .  snd) xs)
