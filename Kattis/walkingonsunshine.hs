import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = do
    ([_,_,yc,_,ya],ys) <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt) >>> head &&& (tail >>> map (\[_,y1,_,y2] -> (y2,y1)) >>> sortOn Down))
    print (walk ya yc ys)

walk :: Int -> Int -> [(Int,Int)] -> Int
walk ya c _ | ya >= c = 0
walk ya c [] = c - ya
walk ya c ((y2,y1):ys) = min (c-ya) (max 0 (c-y2) + walk ya (min c y1) ys)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
