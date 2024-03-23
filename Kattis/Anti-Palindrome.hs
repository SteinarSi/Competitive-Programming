import           Control.Arrow ((>>>))
import           Data.Char     (isAlpha, toLower)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    xs <- getLine <&> (filter isAlpha >>> map toLower)
    putStrLn $ if or (zipWith (==) xs (tail xs) ++ zipWith (==) xs (drop 2 xs))
        then "Palindrome"
        else "Anti-palindrome"
