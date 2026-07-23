import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [[n,l],ds,ws] <- C.getContents <&> (C.lines >>> map (C.words >>> map (readInt >>> fromIntegral)))

    let
        possible :: Double -> Bool
        possible v = pos 0 0 (zip ds ws)
          where
            pos :: Double -> Double -> [(Double,Double)] -> Bool
            pos p t []         = t + (l-p) / v <= l
            pos p t ((d,w):xs) = pos d (w + max d (t + (d-p) / v)) xs

        bin :: Double -> Double -> Double
        bin lo hi
            | hi-lo < 0.0001 = mi
            | possible mi    = bin lo mi
            | otherwise      = bin mi hi
          where
            mi = (lo + hi) / 2

    putStrLn $ if possible 300000000
        then show (bin 0 300000000)
        else "IMPOSSIBLE"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
