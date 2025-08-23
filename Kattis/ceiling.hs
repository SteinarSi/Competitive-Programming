import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Data.List             (foldl', nub)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words
            >>> map readInt
            >>> foldl' insert Empty)
        >>> nub
        >>> length
        >>> print
    )

data Tree = Empty | Node Tree Int Tree

instance Eq Tree where
    (==) Empty Empty = True
    (==) (Node l1 x r1) (Node l2 y r2) = l1 == l2 && r1 == r2
    (==) _ _ = False

insert :: Tree -> Int -> Tree
insert Empty v = Node Empty v Empty
insert (Node l x r) v | v < x     = Node (insert l v) x r
                      | otherwise = Node l x (insert r v)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
