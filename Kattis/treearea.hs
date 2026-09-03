import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map (C.readInt >>> fromJust >>> fst >>> fromIntegral))
        >>> solve
        >>> print
    )

solve :: [[Double]] -> Double
solve []        = 0
solve [[b,r,h]] = r*h
solve ([b1,r1,h1]:[b2,r2,h2]:xs) = r1*h1 - r'*h' + solve ([b2,r2,h2]:xs)
  where
    h' = b1+h1-b2
    r' = r1 * h' / h1
