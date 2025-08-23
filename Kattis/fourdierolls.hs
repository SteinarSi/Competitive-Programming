import           Data.List (nub)

main :: IO ()
main = do
    n <- fmap read getLine
    xs <- fmap (nub . words) getLine

    let a | n == length xs = product $ take (4-n) [6-n,6-n-1..]
          | otherwise = 0
        b = 6^(4-n) - a

    putStrLn (show a ++ " " ++ show b)
