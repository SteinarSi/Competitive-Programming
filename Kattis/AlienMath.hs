import qualified Data.ByteString.Char8 as C
import           Data.Functor             ((<&>))
import           Control.Arrow            ((>>>))
import           Data.Maybe               (fromJust)
import           Data.List                (findIndex)
import           Data.Function            ((&))

main :: IO ()
main = convert 
            <$> (C.getLine <&> readInt) 
            <*> (C.getLine <&> C.words) 
            <*> C.getLine 
        >>= print

convert :: Int -> [C.ByteString] -> C.ByteString -> Int
convert n base xs = digits xs
        & reverse
        & zipWith (*) (map (n^) [0..])
        & sum
    where
        digits :: C.ByteString -> [Int]
        digits xs | C.null xs = []
                  | otherwise = let Just d = findIndex (`C.isPrefixOf` xs) base
                                in  d : digits (C.drop (C.length (base!!d)) xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
