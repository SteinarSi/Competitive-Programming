import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    p:k:xs <- C.getContents <&> (C.words >>> tail >>> map readInt)
    zipWith (-) (xs++[k]) (0 : xs)
        & solve p 100
        & print

solve :: Int -> Int -> [Int] -> Double
solve _ _ []         = 0
solve p speed (i:is) = fromIntegral (i * speed) / 100 + solve p (speed + p) is

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
