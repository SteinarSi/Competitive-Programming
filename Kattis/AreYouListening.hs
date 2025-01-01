import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (cx,cy,n):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> (\(a:b:c:_) -> (a,b,c))) )

    map (range (cx,cy)) xs
        & sort
        & (!!2)
        & floor
        & print

range :: (Double,Double) -> (Double,Double,Double) -> Double
range (cx,cy) (x,y,r) = max 0 (sqrt ((cx-x) ^ 2 + (cy-y) ^ 2) - r)

readInt :: Num n => C.ByteString -> n
readInt = C.readInt >>> fromJust >>> fst >>> fromIntegral
