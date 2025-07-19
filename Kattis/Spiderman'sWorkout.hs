import           Control.Arrow         ((>>>))
import           Data.Array            (Array, range)
import           Data.Array.Base       (UArray, elems, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

data Cost = Cost Int | IMPOSSIBLE deriving (Eq,Ord)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (uncurry solve)
        >>> unlines
        >>> putStr
    )

solve :: Int -> UArray Int Int -> String
solve n xs = case dp ! (0,0) of
    IMPOSSIBLE -> "IMPOSSIBLE"
    Cost h     -> backtrack h (0,0)
  where
    lim = sum (elems xs) `div` 2
    rng = ((0,0),(n,lim))

    dp :: Array (Int,Int) Cost
    dp = listArray rng (map f (range rng))

    f :: (Int,Int) -> Cost
    f (i,h) | i == n && h == 0 = Cost 0
            | i == n = IMPOSSIBLE
            | otherwise = min up dn
      where
        up | h+c <= lim = dp ! (i+1,h+c)
           | otherwise  = IMPOSSIBLE
        dn | h-c >= 0   = max (dp ! (i+1,h-c)) (Cost h)
           | otherwise  = IMPOSSIBLE
        c = xs ! i

    backtrack :: Int -> (Int,Int) -> String
    backtrack t (i,h) | i == n    = ""
                      | h-c < 0 || dp ! (i+1,h-c) > Cost t = 'U' : backtrack t (i+1,h+c)
                      | otherwise = 'D' : backtrack t (i+1,h-c)
      where
        c = xs ! i

parse :: [[Int]] -> [(Int,UArray Int Int)]
parse []           = []
parse ([n]:xs:xss) = (n, listArray (0,n-1) xs) : parse xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
