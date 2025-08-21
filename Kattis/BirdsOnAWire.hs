import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    l:d:n:xs <- C.getContents <&> (C.words >>> map readInt)

    print (birds l d 6 (sort xs))

birds :: Int -> Int -> Int -> [Int] -> Int
birds l d p []     = max 0 ((l-6-p+d) `div` d)
birds l d p (x:xs) = max 0 ((x-p) `div` d) + birds l d (x+d) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
