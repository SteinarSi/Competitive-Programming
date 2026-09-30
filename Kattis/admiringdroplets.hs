import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,h]:[s1,y1]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readNum))

    let
        drop :: Double -> Double -> [[Double]] -> Double
        drop p v []         = (h-p) / (1000 * (v/1000000000)**(1/6))
        drop p v ([s,y]:ys) = (y-p) / (1000 * (v/1000000000)**(1/6)) + drop y (v+s) ys

    print (drop y1 s1 xs)

readNum :: Num n => C.ByteString -> n
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
