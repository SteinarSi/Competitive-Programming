import           Data.Functor ((<&>))

main :: IO ()
main = do
    [a,b,c,d] <- getContents <&> words
    print $ if a == c || b == d
        then 1
        else 2
