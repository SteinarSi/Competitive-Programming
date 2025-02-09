import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.IntMap.Lazy      as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map (readInt
            >>> solve
            >>> show)
        >>> unlines
        >>> putStr
    )

solve :: Int -> Int
solve k = fibonacci
        & drop 2
        & zip [2..]
        & search M.empty
    where
        search :: M.IntMap Int -> [(Int,Int)] -> Int
        search seen ((n,x):xs) = case M.lookup x seen of
            Just s  -> s
            Nothing -> search (M.insert x n seen) xs

        fibonacci :: [Int]
        fibonacci = 1 : 1 : zipWith (\x y -> (x+y) `mod` k) fibonacci (tail fibonacci)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
