import           Control.Arrow (first, (>>>))
import           Data.Char     (isAlpha, isSpace)

main :: IO ()
main = getContents >>= (
            filter (isSpace >>> not)
        >>> element
        >>> fst
        >>> format
        >>> unlines
        >>> putStr
    )

data Array = Array [Array] | Word String

format :: Array -> [String]
format (Word x) = [x]
format (Array []) = ["{", "}"]
format (Array xs) = "{" : map ("  "<>) (mapLast init (concatMap (format >>> mapLast (<>",")) xs)) <> ["}"]

element :: String -> (Array, String)
element (' ':xs) = element xs
element ('{':xs) = first Array (list xs)
element (x:xs)   = first Word (span isAlpha (x:xs))

list :: String -> ([Array],String)
list ('}':xs) = ([],xs)
list (' ':xs) = list xs
list (',':xs) = list xs
list xs = let (a,r) = element xs
          in  first (a:) (list r)

mapLast :: (a -> a) -> [a] -> [a]
mapLast f [] = []
mapLast f xs = init xs <> [f (last xs)]
