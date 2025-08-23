import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    C.getContents >>= (
                C.lines
            >>> tail
            >>> mapM_ (C.words
                    >>> map readInt
                    >>> (\(a:b:_) -> a `choose` (b-1))
                    >>> print
                )
        )

choose :: Integer -> Integer -> Integer
choose n k | k > n           = 0
           | k == 0          = 1
           | k > (n `div` 2) = n `choose` (n-k)
           | otherwise       = n * ((n-1) `choose` (k-1)) `div` k

readInt :: C.ByteString -> Integer
readInt = C.readInt >>> fromJust >>> fst >>> fromIntegral
