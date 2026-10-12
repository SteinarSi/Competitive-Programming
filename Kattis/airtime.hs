import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map readInt
        >>> airtime
        >>> print
    )

airtime :: [Int] -> Int
airtime (x:y:z:xs)
    | y-x > z-y = 1 + airtime (y:z:xs)
    | otherwise =     airtime (y:z:xs)
airtime _ = 0

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
