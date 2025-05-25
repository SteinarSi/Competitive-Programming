import           Data.Functor ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read
    let size = n `div` 4
    print (2 * (size * (size-1) + 2))
