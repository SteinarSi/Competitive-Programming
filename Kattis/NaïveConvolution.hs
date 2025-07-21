import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST)
import           Data.Array.ST         (MArray (..), STArray, readArray,
                                        runSTArray, writeArray)
import           Data.Array.Unboxed    (Ix, UArray, elems, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (dropWhileEnd)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [[n,m],xs',ys'] <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let xs = listArray (1,n) xs' :: UArray Int Int
        ys = listArray (1,m) ys' :: UArray Int Int

        zs = dropWhileEnd (==0) $ elems $ runSTArray $ do
            arr <- newArray (1,n+m-1) 0 :: ST s (STArray s Int Integer)
            sequence_ $ do
                i <- [1..n]
                j <- [1..m]
                pure (modifyArray arr (i+j-1) (fromIntegral (xs!i * ys!j)+))
            pure arr

    putStrLn $ if null zs
        then "0"
        else unwords (map show zs)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
