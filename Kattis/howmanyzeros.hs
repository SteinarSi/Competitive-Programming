import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words >>> map readInt >>> \[m,n] -> show (zerosInRange m n))
        >>> unlines
        >>> putStr
    )

zerosInRange :: Int -> Int -> Int
zerosInRange 0 n = 1 + zerosUpTo n
zerosInRange m n = zerosUpTo n - zerosUpTo (m-1)

zerosUpTo :: Int -> Int
zerosUpTo n
    | n < 10    = 0
    | r == 9    = 10 * zerosUpTo q + q
    | otherwise = zerosIn n + zerosUpTo (n-1)
  where
    (q,r) = quotRem n 10

zerosIn :: Int -> Int
zerosIn 0 = 0
zerosIn n
    | r == 0    = zerosIn q + 1
    | otherwise = zerosIn q
  where
    (q,r) = quotRem n 10

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
