import           Data.Char (isLower, isUpper, toLower, toUpper)

main :: IO ()
main = interact (map (\x -> if isUpper x then toLower x else if isLower x then toUpper x else x))
