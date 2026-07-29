import           Text.Printf (printf)

main :: IO ()
main = do
    f <- read <$> getLine
    printf "%12.2f\n" (f::Double)
