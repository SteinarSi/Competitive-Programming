import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (readInt >>> solve)
        >>> mapM_ print
    )

solve :: Int -> Int
solve n | n >= 5    = 0
        | otherwise = fac n `mod` 10

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
