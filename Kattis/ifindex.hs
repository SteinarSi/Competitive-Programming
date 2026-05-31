import           Control.Arrow ((>>>))
import           Data.List     (sort)

main :: IO ()
main = getLine >>= (
            parse 0
        >>> annoyance
        >>> print
    )

annoyance :: [Int] -> Int
annoyance [] = -1
annoyance xs = sort xs !! ((length xs + 1) `div` 2 - 1)

parse :: Int -> String -> [Int]
parse _ []           = []
parse i ('{':xs)     = parse (i+1) xs
parse i ('}':xs)     = parse (i-1) xs
parse i ('i':'f':xs) = i : parse i xs
parse i xs           = error ("bruh: " <> show (i,xs))
