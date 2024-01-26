import           Control.Monad (forM_, when)

main :: IO ()
main = do
    s <- fmap read getLine
    putStrLn (show s ++ ":")
    forM_ [2..ceiling (fromIntegral s / 2)] $ \a -> do
        when (attempt s a (a-1)) (putStrLn (show a ++ "," ++ show (a-1)))
        when (attempt s a a) (putStrLn (show a ++ "," ++ show a))

attempt :: Int -> Int -> Int -> Bool
attempt s a b = s `mod` (a+b) == 0 || (s-a) `mod` (a+b) == 0
