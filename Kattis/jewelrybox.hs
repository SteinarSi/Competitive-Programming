import           Control.Monad (replicateM_)

main :: IO ()
main = do
    n <- fmap read getLine
    replicateM_ n $ do
        [x, y] <- fmap (map read . words) getLine
        let a = 12
            b = -4 * (x + y)
            c = x * y
            h = (-b - sqrt (b**2 - 4*a*c)) / (2 * a) -- abc formula
            volume = h * (x - 2*h) * (y - 2*h)
        print volume
