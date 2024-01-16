import           Control.Monad (when)

main :: IO ()
main = do
    radius:marked:points:_ <- fmap (map read . words) getLine
    when (radius + marked + points > 0) $ do
        putStrLn (show (radius^2 * pi) ++ " " ++ show ((2 * radius) ^ 2 * points / marked))
        main
