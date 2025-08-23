import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.filter (/=' ')
            >>> splitOn ','
            >>> map readDouble
            >>> sum
            >>> show
            >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

splitOn :: Char -> C.ByteString -> [C.ByteString]
splitOn c x | C.null x  = []
            | otherwise = let (a, b) = C.span (c/=) x
                          in  a : splitOn c (C.dropWhile (c==) b)

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where Just (int, r1) | C.head s == '.' = Just (0, s)
                         | otherwise = C.readInt s
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
