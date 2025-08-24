import           Control.Arrow ((>>>))
import           Control.Monad (ap)
import           Data.Char     (chr, ord)

main :: IO ()
main = interact (
            lines
        >>> map (map (ord >>> subtract (ord 'a')))
        >>> ap (head >>> zipWith decrypt) (last >>> cycle)
        >>> map ((+ ord 'a') >>> chr)
    )

decrypt :: Int -> Int -> Int
decrypt m k | even (m+k) = (m-k) `mod` 26
            | otherwise  = (m+k) `mod` 26
