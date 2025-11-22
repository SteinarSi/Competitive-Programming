import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [_,k]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    print $ runST $ do
        cnt <- newArray (0,23) 0 :: ST s (STUArray s Int Int)
        forM_ xs $ \[a,b] -> forM_ [a..b-1] $ \t -> readArray cnt t >>= (succ >>> writeArray cnt t)
        getElems cnt <&> (filter (>=k) >>> length)


readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
