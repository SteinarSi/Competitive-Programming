import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [s,p] <- C.getContents <&> (C.words >>> map (readInt >>> fromIntegral))
    print (simulate s p 0)

simulate :: Double -> Double -> Double -> Double
simulate s p current | next > current = simulate s p next
                     | otherwise      = current + s
    where
        next = current + s - (current + s ) * p / 100

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
