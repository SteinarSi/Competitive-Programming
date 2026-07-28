import           Data.Function ((&))

main :: IO ()
main = do
    r <- read <$> getLine
    iterate (r*) 1
        & take 4
        & sum
        & round
        & print
