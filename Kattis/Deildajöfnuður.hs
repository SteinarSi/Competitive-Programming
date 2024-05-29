import           Control.Arrow ((>>>))
import           Data.Char     (ord)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import qualified Data.IntMap   as M
import           Data.List     (group)

main :: IO ()
main = getLine >> getLine >>= (
            map (ord >>> subtract (ord 'a'))
        >>> search
        >>> print
    )

search :: [Int] -> Int
search [] = 0
search xs = max (search' M.empty xs) (search (tail xs))
    where
        search' :: M.IntMap Int -> [Int] -> Int
        search' count [] = grant count
        search' count (x:xs) = max (grant count) (search' (M.insertWith (+) x 1 count) xs)

grant :: M.IntMap Int -> Int
grant count = case M.elems count & filter (>0) & group of
    [xs] -> sum xs
    _    -> 0

