import           Data.Functor ((<&>))

main :: IO ()
main = do
    n <- getContents <&> read

    print $ if n `mod` 7 == 0
        then n
        else n+1
