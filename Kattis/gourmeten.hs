import           Control.Arrow         ((>>>), (&&&))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> map readInt
        >>> head &&& drop 2
        >>> uncurry (gourmet [])
        >>> print
    )

gourmet :: [Int] -> Int -> [Int] -> Int
gourmet as 0 _      = round (exp (logFac (sum as) - sum (map logFac as)))
gourmet _  _ []     = 0
gourmet as m (x:xs) = [1..]
        & takeWhile (\a -> m - a*x >= 0)
        & map (\a -> gourmet (a:as) (m-a*x) xs)
        & (gourmet as m xs:)
        & sum

logFac :: Int -> Double
logFac x = [1..x]
        & map (fromIntegral >>> log)
        & sum

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
