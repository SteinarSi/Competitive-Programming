import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = do
    [[n],as,ss] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let ans = zipWith3 (\a s i -> fromIntegral (a+s) / fromIntegral i) as (scanl1 (+) (sortOn Down ss)) [1..n]
    print (maximum ans)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
