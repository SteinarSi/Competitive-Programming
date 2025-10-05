import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (ap)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.List             (maximumBy, sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:x:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))
    let s = maximumBy (compare `on` sqdist x) xs
    sortOn (sqdist s) (x:xs)
        & ap (zipWith dist) tail
        & sum
        & print

sqdist :: (Int,Int) -> (Int,Int) -> Int
sqdist (a,b) (x,y) = (a-x)^2 + (b-y)^2

dist :: (Int,Int) -> (Int,Int) -> Double
dist = sqdist >>> (>>>fromIntegral >>> sqrt)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
