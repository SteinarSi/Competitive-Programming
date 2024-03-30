import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)

main :: IO ()
main = C.getContents >>= (C.lines
    >>> tail
    >>> mapM_ (C.unpack
        >>> solve
        >>> print
        )
    )

solve :: String -> Double
solve xs = fromIntegral (length xs) + sum (zipWith dist xs (tail xs))

dist :: Char -> Char -> Double
dist a b = time
    where angle = fromIntegral diff * 2 * pi / 28
          diff = min ((index a - index b) `mod` 28) ((index b - index a) `mod` 28)
          dist = fromIntegral diff * 60 * pi / 28
          time = dist / 15

index :: Char -> Int
index '\'' = 27
index ' '  = 26
index  c   = ord c - ord 'A'
