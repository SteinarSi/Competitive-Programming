import           Data.Functor ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read

    print $ if n < 1
        then - (n * (n - 1) `div` 2) + 1
        else n * (n+1) `div` 2
