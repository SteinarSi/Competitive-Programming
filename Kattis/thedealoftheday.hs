import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getLine <&> (C.words >>> map readInt >>> filter (>0))
    k <- C.getLine <&> readInt
    print (combs k xs)

combs :: Integer -> [Integer] -> Integer
combs 0 _  = 1
combs _ [] = 0
combs k (x:xs) = case compare k (fromIntegral (length (x:xs))) of
    LT -> x * combs (k-1) xs + combs k xs
    EQ -> x * combs (k-1) xs
    GT -> 0

readInt :: C.ByteString -> Integer
readInt = C.readInt >>> fromJust >>> fst >>> fromIntegral
