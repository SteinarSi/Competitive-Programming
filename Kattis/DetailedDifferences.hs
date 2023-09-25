import Data.Bool (bool)
import Data.Char (isAlphaNum)

main :: IO ()
main = getLine >>= solve . read

solve :: Int -> IO ()
solve 0 = pure ()
solve n = do
    a <- fmap (filter isAlphaNum) getLine
    b <- fmap (filter isAlphaNum) getLine
    putStrLn a
    putStrLn b
    putStrLn $ zipWith (\a b -> bool '*' '.' (a==b)) a b
    putChar '\n'
    solve (n-1)

    