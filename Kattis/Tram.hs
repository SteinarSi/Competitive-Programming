import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    points <- C.getContents <&> (
                C.lines
            >>> tail
            >>> map (C.words
                >>> map readNum
                >>> (\(a:b:_) -> (a,b))
            )
        )
    print $ gradientDescent points 200000 500000.0

gradientDescent :: [(Double,Double)] -> Double -> Double -> Double
gradientDescent xs t a | t <= 0.001 = a
                       | u_left <= u_middle && u_left <= u_middle = gradientDescent xs (t * 2 / 3) left
                       | u_right <= u_middle && u_right <= u_left = gradientDescent xs (t * 2 / 3) right
                       | otherwise = gradientDescent xs (t / 2) a
    where
        left  = a - t
        right = a + t
        u_left   = unusefulness left
        u_middle = unusefulness a
        u_right  = unusefulness right

        unusefulness :: Double -> Double
        unusefulness a = map (distance a) xs & sum

distance :: Double -> (Double,Double) -> Double
distance a (x,y) = (x+a) - y
    & abs
    & (/sqrt 2)
    & (^^2)

readNum :: Num a => C.ByteString -> a
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
