import           Control.Arrow         ((***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (readInt >>> kaprekar >>> bool "NO" "YES")
        >>> unlines
        >>> putStr
    )

kaprekar :: Int -> Bool
kaprekar 1 = True
kaprekar x = any ((`splitAt` xs) >>> read *** read >>> \(a,b) -> b > 0 && a+b == x) [1..length xs-1]
  where
    xs = show (x*x)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
