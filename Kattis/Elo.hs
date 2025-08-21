import           Control.Arrow         ((&&&), (>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
        C.lines
    >>> map (C.words >>> map readInt)
    >>> (head >>> last)
            &&&
        (tail >>> map (\[l,r,a] -> (l,r,a)) >>> sort)
    >>> uncurry solve
    >>> print)

solve :: Int -> [(Int,Int,Int)] -> Int
solve x players = dp ! x
  where
    dp = listArray (x,5500) (map f [x..5500])
    f e = maximum (elos e players)

    elos :: Int -> [(Int,Int,Int)] -> [Int]
    elos e [] = [e]
    elos e ((l,r,a):xs)
        | l > e     = [e]
        | e <= r    = dp ! (e+a) : elos e xs
        | otherwise =              elos e xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
