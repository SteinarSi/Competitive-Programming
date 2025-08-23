import           Control.Monad (forM_, guard)

main :: IO ()
main = do
    [a, b, c, d] <- fmap (map read . words) getLine

    let solulus = do
            (n1,op1) <- ops
            guard (n1 /= "/" || b /= 0)
            (n2,op2) <- ops
            guard (n2 /= "/" || d /= 0)
            guard (a `op1` b == c `op2` d)
            pure (n1, n2)

    if null solulus
        then putStrLn "problems ahead"
        else forM_ solulus $ \(n1,n2) -> putStrLn $ unwords [show a, n1, show b, "=", show c, n2, show d]

ops :: [(String, Int -> Int -> Int)]
ops = [("*", (*)), ("+", (+)), ("-",(-)), ("/", div)]
