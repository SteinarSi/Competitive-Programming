import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)
    let (rest,top) = sort xs
            & splitAt (n+1-k)
    print $ if k >= n
        then average xs
        else (fromIntegral (sum top) + average rest) / fromIntegral k

average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
