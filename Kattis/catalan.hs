import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map (readInteger >>> (catalan !) >>> show)
        >>> unlines
        >>> putStr
    )

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst

catalan :: Array Integer Integer
catalan = listArray (0,5000) (map cat [0..5000])
  where
    cat :: Integer -> Integer
    cat 0 = 1
    cat n = 2 * (2*n-1) * catalan ! (n-1) `div` (n+1)
