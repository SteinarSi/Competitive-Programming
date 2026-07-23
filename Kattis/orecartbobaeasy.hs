import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [[n,l],ds,ws,[v]] <- C.getContents <&> (C.lines >>> map (C.words >>> map (readInt >>> fromIntegral)))

    let
        possible :: Double -> Double -> [(Double,Double)] -> Bool
        possible p t []         = t + (l-p) / v <= l
        possible p t ((d,w):xs) = possible d (w + max d (t + (d-p) / v)) xs

    putStrLn $ if possible 0 0 (zip ds ws)
        then "YES"
        else "NO"

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
