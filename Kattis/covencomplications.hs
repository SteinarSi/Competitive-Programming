import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getElems, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, elems, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,_]:fs:cs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        ferrets :: UArray Int Int
        ferrets = listArray (1,n) fs

        losses = runST (do
                ret <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
                forM_ cs $ \[u,v] -> do
                    let fu = ferrets ! u
                        fv = ferrets ! v
                    when (fu == 0 && fv > 0) (readArray ret u >>= (((fv)+) >>> writeArray ret u))
                    when (fu > 0 && fv == 0) (readArray ret v >>= (((fu)+) >>> writeArray ret v))
                getElems ret)
            & filter (/=0)
            & sort

    print $ case losses of
        []  -> 0
        [_] -> 0
        xs -> init xs
            & scanl1 (+)
            & sum

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
