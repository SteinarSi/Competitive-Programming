import           Data.Functor ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read
    print $ length [() | h <- [5..23], w <- [4..23], 2 * (h + w-2) <= n]
