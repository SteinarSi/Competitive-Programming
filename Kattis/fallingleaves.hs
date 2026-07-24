import           Control.Arrow (second, (***), (>>>))
import           Data.Function ((&))

data Tree = Node Char Tree Tree | Empty

main :: IO ()
main = getContents >>= (
            lines
        >>> init
        >>> parse
        >>> map (construct >>> preorder)
        >>> unlines
        >>> putStr
    )

preorder :: Tree -> String
preorder Empty        = ""
preorder (Node x l r) = x : preorder l <> preorder r

construct :: [String] -> Tree
construct []       = Empty
construct [[x]]    = Node x Empty Empty
construct (xs:xss) = foldr insert (construct xss) xs

insert :: Char -> Tree -> Tree
insert x Empty = Node x Empty Empty
insert x (Node c l r)
    | x < c     = Node c (insert x l) r
    | otherwise = Node c l (insert x r)

parse :: [String] -> [[String]]
parse [] = []
parse xs = span (/="*") xs
    & second (drop 1 >>> parse)
    & uncurry (:)
