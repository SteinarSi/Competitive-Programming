import           Control.Arrow         ((>>>))
import           Data.Bits             (shiftR)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:k:xs <- C.getContents <&> (C.words >>> map readInt)
    map twos xs
        & sum
        & (>=k)
        & bool 0 1
        & print

twos :: Int -> Int
twos 0 = 0
twos x | even x    = 1 + twos (x `shiftR` 1)
       | otherwise = 0

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
