import Data.Functor ((<&>))
import Data.List (isSubsequenceOf)

main :: IO ()
main = do
    [a,b] <- getContents <&> words
    putStrLn $ if a `isSubsequenceOf` b
        then "Ja"
        else "Nej"
