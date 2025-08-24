import           Control.Arrow ((>>>))
import           Data.Char     (digitToInt, isDigit)
import           Text.Printf   (printf)

main :: IO ()
main = getLine >>= (translate >>> putStrLn)

translate :: String -> String
translate [] = []
translate (x:xs)
    | x `elem` "aeiou" = x : translate xs
    | isDigit x = printf "%b" (digitToInt x) <> translate xs
    | x == '-' = '\n' : translate xs
    | otherwise = "beepbloop" <> translate xs
