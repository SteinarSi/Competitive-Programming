import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (foldl')
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust, fromMaybe)

data Trie = Node {
        ends     :: Int,
        children :: M.Map Char Trie
    }
    deriving Show

empty :: Trie
empty = Node {
        ends = 0,
        children = M.empty
    }

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.lines

    let (trie, queries) =
                map C.unpack rest
            & splitAt (readInt n)
            & foldl' insert empty *** tail

    queries
        & map (count trie >>> show)
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

insert :: Trie -> String -> Trie
insert trie []     = trie {ends = ends trie + 1}
insert Node {ends, children} (x:xs) = Node {
            ends     = ends + 1,
            children = M.insert x (insert child xs) children
        }
    where
        child = fromMaybe empty (M.lookup x children)

count :: Trie -> String -> Int
count trie [] = ends trie
count trie (x:xs) = case M.lookup x (children trie) of
        Nothing    -> 0
        Just child -> count child xs

parse :: [String] -> Trie
parse = foldl' insert empty
