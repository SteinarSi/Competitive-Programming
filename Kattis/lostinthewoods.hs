import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_)
import           Data.Array.ST         (MArray, newArray, readArray, runSTArray,
                                        runSTUArray, writeArray)
import           Data.Array.Unboxed    (Ix, UArray, assocs, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            ret <- newArray (0,n-1) []
            forM_ xs $ \[u,v] -> do
                modifyArray ret u (v:)
                modifyArray ret v (u:)
            pure ret
        vs = assocs graph
            & filter (snd >>> null >>> not)
            & map fst
            & filter (/=n-1)

        next :: UArray Int Double -> UArray Int Double
        next ch =
            runSTUArray $ do
            ret <- newArray (0,n-1) 0
            forM_ vs $ \u -> let x = ch ! u / fromIntegral (length (graph ! u))
                             in forM_ (graph ! u) (\v -> modifyArray ret v (x+))
            pure ret

    listArray (0,n-1) (1 : repeat 0)
        & iterate next
        & take 10000
        & zipWith (\t ch -> fromInteger t * ch ! (n-1)) [0..]
        & sum
        & print

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
