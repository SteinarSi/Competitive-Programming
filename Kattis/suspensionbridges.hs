import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [d,s] <- getLine <&> (words >>> map read)

    let
        search :: Double -> Double -> Double
        search lo hi
            | abs (lo-hi) <= 0.0000000001 = lo
            | s1 <= s2  = search lo mi2
            | otherwise = search mi1 hi
          where
            w = (hi-lo) / 3
            mi1 = lo + w
            mi2 = hi - w
            s1 = score mi1
            s2 = score mi2

        score :: Double -> Double
        score x = abs (x * cosh (d / (2*x)) - x - s)

        a = search 0.0001 10000000

    print (2*a * sinh (d / (2*a)))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
