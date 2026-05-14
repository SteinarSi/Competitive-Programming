import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.List     (groupBy)

main :: IO ()
main = getContents >>= (
            words
        >>> drop 1
        >>> map encode
        >>> unwords
        >>> putStrLn
    )

encode :: String -> String
encode = groupBy (\x y -> vowel x == vowel y) >>> concatMap (\xs@(x:_) -> bool xs ('i':'b':xs) (vowel x))

vowel :: Char -> Bool
vowel = (`elem` "aeiou")
