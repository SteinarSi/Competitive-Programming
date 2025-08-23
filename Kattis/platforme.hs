import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Ix               (inRange)
import           Data.List             (find, sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (..))

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words
            >>> map readInt
            >>> (\(y:x1:x2:_) -> (y, (2*x1, 2*x2))))
        >>> sortOn Down
        >>> build
        >>> print
    )

build :: [(Int,(Int,Int))] -> Int
build xs = xs
    & map (\(y,(a,b)) -> support y (a+1) + support y (b-1))
    & sum
    where
        support :: Int -> Int -> Int
        support y x = xs
            & find (((fst >>> (<y))
                        &&&
                     (snd >>> (`inRange` x)))
                >>> uncurry (&&))
            & maybe 0 fst
            & (y-)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
