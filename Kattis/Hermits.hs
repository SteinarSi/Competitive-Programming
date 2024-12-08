import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray,
                                        newListArray, readArray, writeArray)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import           Data.List             (minimumBy)
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)

main :: IO ()
main = do
    [n]:as:_:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let people = listArray (1,n) as :: UArray Int Int

    print $ runST $ do
        busy <- newListArray (1,n) as :: ST s (STUArray s Int Int)
        forM_ xs $ \[x,y] -> do
            readArray busy x >>= ((+ (people ! y)) >>> writeArray busy x)
            readArray busy y >>= ((+ (people ! x)) >>> writeArray busy y)
        getAssocs busy <&> (minimumBy (compare `on` swap) >>> fst)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
