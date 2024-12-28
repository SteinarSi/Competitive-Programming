import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (tails)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> solulu
        >>> print
    )

solulu :: [Int] -> Int
solulu xs | null xs = 0
          | null sequences = 1
          | otherwise = maximum sequences
    where
        sequences :: [Int]
        sequences = do
            y:ys <- tails xs
            z:zs <- tails ys
            pure (sequence y z zs)

        sequence :: Int -> Int -> [Int] -> Int
        sequence _ _ [] = 2
        sequence x y (z:zs) | x + y == z = 1 + sequence y z zs
                            | otherwise  =     sequence x y zs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
