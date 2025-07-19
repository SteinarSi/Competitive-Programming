import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    (n,t):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map (readInt >>> fromIntegral) >>> head &&& last))
    let
        newton :: (Double,Double) -> (Double,Double)
        newton (a,b) | time c 0 xs > t = (c,b)
                     | otherwise       = (a,c)
          where
            c = (a+b) / 2

    printf "%.6f\n" (fst (iterate newton (-1e9,1e9) !! 200))

time :: Double -> Double -> [(Double,Double)] -> Double
time c ret [] = ret
time c ret ((d,s):xs) | s+c <= 0  = 1e9
                      | otherwise = time c (ret + d / (s+c)) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
