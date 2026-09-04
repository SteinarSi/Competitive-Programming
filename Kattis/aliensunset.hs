import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.lines >>> drop 1 >>> map (C.words >>> map readInt))
    map head xs
        & maximum
        & (*1825)
        & enumFromTo 0
        & find (\t -> all (darkness t) xs)
        & maybe "impossible" show
        & putStrLn

darkness :: Int -> [Int] -> Bool
darkness x [h,r,t]
    | r < t     = x' <= r || t <= x'
    | otherwise = t <= x' && x' <= r
  where
    x' = x `mod` h

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
