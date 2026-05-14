import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import           Data.Ratio            ((%))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> zip [1..]
        >>> filter (fst >>> even)
        >>> map snd
        >>> map (C.words
            >>> map readInteger
            >>> chunksOf 2
            >>> map (\[v,d] -> d % v)
            >>> flip zip [1..]
            >>> minimum
            >>> snd
            >>> show)
        >>> unlines
        >>> putStr
    )

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
