import           Control.Arrow         (first, second, (&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)
import           Data.Ratio            (Ratio, (%))

main :: IO ()
main = do
    (h,xs) <- C.getContents <&> (C.lines >>> drop 1 >>> (head >>> readInt) &&& (tail >>> map (C.words >>> map readInt >>> head &&& last) >>> sort))

    let (l,r) = map (second (subtract h)) xs
            & span (fst >>> (<0))
            & first (map (first negate) >>> reverse)

    print (observable (-99999999) l + observable (-99999999) r)

observable :: Ratio Int -> [(Int,Int)] -> Int
observable _ [] = 0
observable h ((x,y):xs) | y % x > h = 1 + observable (y%x) xs
                 | otherwise = observable h xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
