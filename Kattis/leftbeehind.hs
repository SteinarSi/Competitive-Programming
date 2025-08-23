import           Control.Monad (when)

main :: IO ()
main = do
    [x, y] <- fmap (map read . words) getLine
    when ((x,y) /= (0,0)) $ do
        putStrLn $ case compare x y of
            _ | x+y == 13 -> "Never speak again."
            GT            -> "To the convention."
            LT            -> "Left beehind."
            EQ            -> "Undecided."
        main
