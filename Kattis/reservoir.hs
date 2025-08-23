import           Control.Arrow         ((***), (>>>))
import           Data.Array.Base       (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> concatMap solve
        >>> map show
        >>> unlines
        >>> putStr
    )

solve :: (Int, [(Int,Int)], [Int]) -> [Int]
solve (n, xs, qs) = map (\q -> bin q 0 n) qs
  where
    capacity :: UArray Int Int
    capacity = listArray (0,n) (0 : caps [] xs)

    caps :: [(Int,Int,Int,Int)] -> [(Int,Int)] -> [Int]
    caps _ [] = []
    caps ps ((x,y):xs) = c : caps ps' xs
      where
        (c,ps') = search 0 ps

        search :: Int -> [(Int,Int,Int,Int)] -> (Int,[(Int,Int,Int,Int)])
        search d [] = (x*y-d,[(x,y,x*y-d,d+y)])
        search d ((px,py,pc,pd):xs) = let c' = pc+y*(x-px-1)-d
                                      in  case compare py y of
            GT -> (c', (x,y,c',d+y):(px,py,pc,pd):xs)
            EQ -> (c', (x,y,c',pd+d+y):xs)
            LT -> search (d+pd) xs

    bin :: Int -> Int -> Int -> Int
    bin q lo hi | lo >= hi          = lo
                | capacity ! mi < q = bin q mi hi
                | otherwise         = bin q lo (mi-1)
      where
        mi = (lo+hi+1) `div` 2

parse :: [[Int]] -> [(Int, [(Int,Int)], [Int])]
parse [] = []
parse ([n]:xs:ys:[q]:rest) = splitAt q rest
    & (map head >>> (n, zip xs ys,)) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
