import           Control.Arrow ((>>>))
import           Data.Bool     (bool)

main :: IO ()
main = getContents >>= (
            lines
        >>> init
        >>> map (\x -> '<' : x <> "> is " <> bool "not " "" (acceptable x) <> "acceptable.")
        >>> unlines
        >>> putStr
    )

vowel :: Char -> Bool
vowel = (`elem` "aeiou")

acceptable :: String -> Bool
acceptable xs = any vowel xs && accept xs
  where
    accept :: String -> Bool
    accept []       = True
    accept [_]      = True
    accept (x:y:z:xs) | vowel x == vowel y && vowel y == vowel z = False
    accept (x:y:xs) = (x /= y || x == 'e' || x == 'o') && accept (y:xs)
