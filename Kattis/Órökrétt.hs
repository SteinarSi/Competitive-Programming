import           Control.Arrow    ((***))
import           Control.Category ((>>>))
import           Data.Bool        (bool)
import           Data.Function    ((&))
import           Data.Functor     ((<&>))

main :: IO ()
main = interact (parse >>> satisfiable >>> bool "Neibb\n" "Jebb\n")

satisfiable :: [[String]] -> Bool
satisfiable = any (\xs -> all (\x -> ('!':x) `notElem` xs) xs)

parse :: String -> [[String]]
parse ""     = []
parse ('(':xs) = span (/=')') xs
        & (words >>> filter (/="OG")) *** parse
        & uncurry (:)
parse (_:xs) = parse xs
