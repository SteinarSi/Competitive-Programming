import           Data.Function ((&))

main :: IO ()
main = do
    t <- read <$> getLine
    [1..]
        & takeWhile (\n -> ((n+1)*n) `div` 2 <= t)
        & last
        & print
