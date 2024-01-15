import           Control.Monad (forM_)

main :: IO ()
main = do
    n:_:legs <- fmap (map read . words) getContents
    forM_ [1..n] (print . ghost (reverse legs))

ghost :: [Int] -> Int -> Int
ghost [] a = a
ghost (x:xs) a | a == x = ghost xs (x+1)
               | a == x + 1 = ghost xs x
               | otherwise = ghost xs a
