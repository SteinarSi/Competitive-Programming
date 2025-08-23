import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray (..), STUArray, UArray, freeze,
                                        readArray, writeArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (Ix)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    points <- C.getContents <&> (C.lines >>> drop 1 >>> map (C.words >>> map readInt >>> head &&& last))
    let (xs,ys) = count points
    points
        & map (\(x,y) -> (xs ! x - 1) * (ys ! y - 1))
        & sum
        & print

count :: [(Int,Int)] -> (UArray Int Int,UArray Int Int)
count points = runST $ do
    xx <- newArray (1,100000) 0 :: ST s (STUArray s Int Int)
    yy <- newArray (1,100000) 0 :: ST s (STUArray s Int Int)
    mapM_ (\(x,y) -> modifyArray xx x succ >> modifyArray yy y succ) points
    xs <- freeze xx
    ys <- freeze yy
    pure (xs,ys)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

