import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Foldable         (foldlM)
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:_:xs <- C.getContents <&> (C.words >>> map readInt)

    let (pairs,singles) = count n xs

    print $ pairs + case (singles,n - pairs - singles) of
        (1,0) -> 1
        (0,1) -> 1
        _     -> 0

count :: Int -> [Int] -> (Int,Int)
count n xs = runST $ do
    seen <- newArray (1,n) False :: ST s (STUArray s Int Bool)
    foldlM (\(p,s) x ->
        readArray seen x >>= bool (writeArray seen x True >> pure (p,s+1)) (pure (p+1,s-1))
        ) (0,0) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
