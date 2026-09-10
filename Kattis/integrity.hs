import           Control.Arrow         ((***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map solve
        >>> unlines
        >>> putStrLn
    )

solve :: (Int,Int,[[Int]]) -> String
solve (b,k,xs) = map (validate 0 >>> bool '0' '1') xs
  where
    validate :: Int -> [Int] -> Bool
    validate curr []     = curr `mod` k == 0
    validate curr (y:ys) = y <= b-1 && validate (curr*b + y) ys

parse :: [[Int]] -> [(Int,Int,[[Int]])]
parse ([b]:[m]:[k]:xs) = splitAt m xs
    & (map init >>> (b,k,)) *** parse
    & uncurry (:)
parse _ = []

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
