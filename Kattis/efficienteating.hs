import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> drop 1 >>> wait 0)
        >>> flip zip [0..]
        >>> minimum
        >>> snd
        >>> print
    )

wait :: Int -> [Int] -> Int
wait c []  = c
wait c [_] = error "bruh"
wait c (t:w:xs)
    | 0 < w && w < c = wait c xs
    | otherwise      = wait (c+t) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
