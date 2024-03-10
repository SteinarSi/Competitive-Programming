main :: IO ()
main = getLine >>= solve . read

solve :: Int -> IO ()
solve n = do
    putStrLn $ space (n+1) ++ "x"
    mapM_ (putStrLn . put '/' '\\') [n,n-1..1]
    putStrLn $ put 'x' 'x' 0
    mapM_ (putStrLn . put '\\' '/') [1..n]
    putStrLn $ space (n+1) ++ "x"

    where
        space :: Int -> String
        space j = replicate j ' '

        put :: Char -> Char -> Int -> String
        put a b i = space i ++ [a] ++ space (n*2+3 - 2*(i+1)) ++ [b]
