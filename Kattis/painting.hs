import           Text.Printf (printf)

main :: IO ()
main = do
    d <- fmap read getLine
    printf "%.1f\n" (50 * tan (d * pi / 180) :: Double)
