import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromJust)

main :: IO ()
main = B.getContents >>= (
                B.lines
            >>> tail
            >>> mapM_ (
                    B.readInt
                    >>> fromJust
                    >>> fst
                    >>> digits
                    >>> oneLess
                    >>> zipWith (*) (map (10^) [0..])
                    >>> sum
                    >>> print
                )
        )

digits :: Int -> [Int]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

oneLess :: [Int] -> [Int]
oneLess []     = error "bruh"
oneLess (0:xs) = 0 : oneLess xs
oneLess (x:xs) = pred x : xs
