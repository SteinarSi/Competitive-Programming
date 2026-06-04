import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (intercalate, sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (uncurry solve >>> format)
        >>> unlines
        >>> putStr
    )

format :: [Int] -> String
format [] = "none"
format xs = intercalate "," (map show xs)

solve :: (Int,Int) -> [Int] -> [Int]
solve (l,r) = search 1 >>> filter (>=l) >>> sort
  where
    search :: Int -> [Int] -> [Int]
    search x []     = [x]
    search x (p:ps) = pick <> search x ps
      where
        pick | p*x <= r  = search (p*x) (p:ps)
             | otherwise = []

parse :: [[Int]] -> [((Int,Int),[Int])]
parse []              = []
parse [_]             = []
parse (_:ps:[l,r]:xs) = ((l,r),ps) : parse xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
