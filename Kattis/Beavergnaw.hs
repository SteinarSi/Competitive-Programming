import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> mapM_ (
                C.words
            >>> map readNum
            >>> head &&& last
            >>> uncurry beavergnaw
            >>> print
        )
    )

beavergnaw :: Double -> Double -> Double
beavergnaw d v = (d**3 - 6*v / pi) ** (1 / 3)

readNum :: Num a => C.ByteString -> a
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
