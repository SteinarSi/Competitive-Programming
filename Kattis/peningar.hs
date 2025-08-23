import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (readArray, writeArray)
import           Data.Array.ST         (STUArray, newListArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:d:xs <- C.getContents <&> (C.words >>> map readInt)
    print $ runST (newListArray (0,n-1) xs >>= walk n d 0 0)

walk :: Int -> Int -> Int -> Int -> STUArray s Int Int -> ST s Int
walk n d i r arr = do
    x <- readArray arr i
    if x == -1
        then pure r
        else writeArray arr i (-1) >> walk n d ((i+d) `mod` n) (r+x) arr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
