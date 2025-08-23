import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words
            >>> map readInt
            >>> (\(p:r:f:_) -> brute p r f)
            >>> show)
        >>> unlines
        >>> putStr
    )

brute :: Int -> Int -> Int -> Int
brute p r f = force p
    where
        force x | x > f     = 0
                | otherwise = 1 + force (r*x)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
