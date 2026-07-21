import           Control.Arrow         ((***), (>>>))
import           Data.Array            (Array, listArray, range, (!))
import           Data.Bits             (bit, popCount, setBit, shiftL, testBit,
                                        (.&.), (.|.))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.List             (foldl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (uncurry solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: Int -> Integer -> Int
solve n = solulu (n-1)
  where
    solulu :: Int -> Integer -> Int
    solulu (-1) _ = 1
    solulu 0 mask = n - popCount (mask .&. ((1 `shiftL` n) - 1))
    solulu r mask = [0..n-1]
        & filter (\c -> not (testBit mask (r*n+c)))
        & map (\c -> solulu (r-1) (mask .|. masks ! (n,r,c)))
        & sum

masks :: Array (Int,Int,Int) Integer
masks = listArray rng (map f (range rng))
  where
    rng = ((1,0,0),(12,11,11))
    f (n,r,c)
        | r >= n || c >= n = 0
        | otherwise = let left  = foldl' (\m i  -> setBit m ((r-i)*n+c-i)) 0 [1..min c r]
                          right = foldl' (\m i  -> setBit m ((r-i)*n+c+i)) 0 [1..min r (n-1-c)]
                          up    = foldl' (\m r' -> setBit m (r'*n+c)) 0 [0..r-1]
                      in  left .|. right .|. up

parse :: [[Int]] -> [(Int, Integer)]
parse [] = []
parse ([n,m]:xs) = splitAt m xs
    & (map (\[r,c] -> bit (n*r+c)) >>> sum >>> (n,)) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
