import           Control.Arrow         (first, second, (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> mapM_ (C.words
            >>> split
            >>> first (mean >>> show >>> C.pack)
            >>> uncurry (:)
            >>> C.unwords
            >>> C.putStrLn
        )
    )

mean :: Fractional f => [f] -> f
mean xs = sum xs / fromIntegral (length xs)

split :: [C.ByteString] -> ([Double], [C.ByteString])
split [] = ([], [])
split (x:xs) | isAlpha (C.head x) = second (x :)            (split xs)
             | otherwise          = first  (readDouble x :) (split xs)

readDouble :: C.ByteString -> Double
readDouble s | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where Just (int, r1) = C.readInt s
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
