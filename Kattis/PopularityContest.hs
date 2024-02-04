import           Control.Arrow         ((>>>))
import           Control.Monad         (forM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray (STUArray))
import           Data.Array.ST         (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as B
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:m:xs <- fmap (map readInt . B.words) B.getContents
    putStrLn $ runST $ do
        friends <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
        forM_ xs $ \i -> readArray friends i >>= (succ >>> writeArray friends i)
        forM [1..n] (\i -> readArray friends i <&> (subtract i >>> show)) <&> unwords

readInt :: B.ByteString -> Int
readInt = B.readInt >>> fromJust >>> fst
