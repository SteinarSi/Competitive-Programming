import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, assocs, listArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isControl)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    C.getContents >>= (
                C.filter (isControl >>> not)
            >>> C.unpack
            >>> listArray ((1,1),(n,n))
            >>> findSpots
            >>> uncurry runToSafety
            >>> print
        )

runToSafety :: [(Int,Int)] -> [(Int,Int)] -> Int
runToSafety homes spies = map (manhattan >>> (`map` homes) >>> minimum) spies & maximum

findSpots :: UArray (Int,Int) Char -> ([(Int,Int)], [(Int,Int)])
findSpots graph = (f 'H' graph, f 'S' graph)
    where
        f c = assocs
            >>> filter (snd >>> (==c))
            >>> map fst

manhattan :: (Int,Int) -> (Int,Int) -> Int
manhattan (a,b) (x,y) = abs (a-x) + abs (b-y)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
