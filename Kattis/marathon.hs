import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    _:xs <- C.getContents <&> (C.words >>> map (C.readInt >>> fromJust >>> fst >>> fromIntegral))
    let dist = worstCase Nothing xs
    printf "%.1f %d\n" dist (remove dist Nothing xs)

remove :: Double -> Maybe Double -> [Double] -> Int
remove d (Just p) [x]
    | 5000000 - p <= d = 1
    | otherwise        = 0
remove d Nothing [x] = 0
remove d Nothing (x:y:xs)
    | y <= d    = 1 + remove d Nothing (y:xs)
    | otherwise =     remove d (Just x) (y:xs)
remove d (Just p) (x:y:xs)
    | (y-p) / 2 <= d  = 1 + remove d (Just p) (y:xs)
    | otherwise       =     remove d (Just x) (y:xs)

worstCase :: Maybe Double -> [Double] -> Double
worstCase (Just p) []     = 5000000 - p
worstCase Nothing (x:xs)  = max x (worstCase (Just x) xs)
worstCase (Just p) (x:xs) = max ((x-p) / 2) (worstCase (Just x) xs)
