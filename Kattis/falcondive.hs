import           Control.Arrow         (second, (&&&), (***), (>>>))
import           Control.Monad         (join)
import           Data.Array.Unboxed    (UArray, assocs, bounds, elems, inRange,
                                        indices, listArray, (!), (//))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    m':n':c':rest <- C.getContents <&> C.words

    let m = readInt m'
        n = readInt n'
        c = C.index c' 1

        (xs,ys) = splitAt m rest
            & join (***) (concatMap C.unpack >>> listArray ((1,1),(m,n)))

        xx@((y1,x1):_) = search c xs
        (y2,x2):_ = search c ys

    (xs // map (id &&& (ys!)) xx)
        & (// filter (fst >>> inRange (bounds xs)) (map (\(y,x) -> ((y+2*(y2-y1),x+2*(x2-x1)),c)) xx))
        & elems
        & chunksOf n
        & unlines
        & putStr

search :: Char -> UArray (Int,Int) Char -> [(Int,Int)]
search c = assocs >>> filter (snd >>> (==c)) >>> map fst

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
