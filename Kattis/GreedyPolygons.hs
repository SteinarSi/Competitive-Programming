import           Control.Monad (replicateM_)

main :: IO ()
main = do
    cases <- fmap read getLine
    replicateM_ cases $ do
        [n, l, d, g] <- fmap (map read . words) getLine
        let delta    = d * g
            internal = n * l**2 * cot (pi / n) / 4
            sides    = n * l * delta
            corners  = delta ** 2 * pi
        print (internal + sides + corners)

cot :: Floating a => a -> a
cot a = 1 / tan a
