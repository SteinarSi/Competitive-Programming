import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort, zip6)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,xs) <- C.getContents <&> (C.words >>> map readInt >>> head &&& (tail >>> sort))
    zip6 [0..] (scanl (+) 0 xs) (scanl max 0 xs) xs [n,n-1..] (scanr (+) 0 xs)
        & map (cost (maximum xs))
        & minimum
        & print

cost :: Int -> (Int,Int,Int,Int,Int,Int) -> Int
cost m (l,lt,lm,x,r,rt) = l * lm - lt + r * m - rt

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
