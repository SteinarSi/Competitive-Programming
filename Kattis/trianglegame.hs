import           Control.Arrow         (second, (>>>))
import           Control.Monad         (guard)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (delete)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> chunksOf 7
        >>> map (init
            >>> map (C.words >>> map readInt >>> \[a,b,c] -> [(a,b,c),(b,c,a),(c,a,b)])
            >>> solve
            >>> format)
        >>> unlines
        >>> putStr
    )

solve :: [[(Int,Int,Int)]] -> [Int]
solve (xs:xss) = concatMap (\(a,b,c) -> solulu a c b xss) xs
  where
    solulu :: Int -> Int -> Int -> [[(Int,Int,Int)]] -> [Int]
    solulu _ _ r []   = []
    solulu f l r [xs] = [c+r | (a,b,c) <- xs, a == f, b == l]
    solulu f l r yss = do
        xs <- yss
        (a,b,c) <- xs
        guard (a == l)
        solulu f c (r+b) (delete xs yss)

format :: [Int] -> String
format [] = "none"
format xs = show (maximum xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
