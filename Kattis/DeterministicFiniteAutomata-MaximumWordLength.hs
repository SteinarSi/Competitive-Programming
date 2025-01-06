import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array)
import           Data.Array.ST         (STUArray, newArray, runSTUArray, writeArray)
import           Data.Array.Base       (UArray, listArray, readArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.IntSet           as S

main :: IO ()
main = do
    ncsf:sigma:fs:tss <- C.getContents <&> C.lines
    let [n,c,s,f] = C.words ncsf
            & map readInt
        table = parseTable n c tss
        accept = parseAccept n fs
    print $ runST $ do
        stack <- newArray (1,n) False
        nub <- newArray (1,n) False
        longestWalk c stack nub table accept 0 s

longestWalk :: Int -> STUArray s Int Bool -> STUArray s Int Bool -> UArray (Int,Int) Int -> UArray Int Bool -> Int -> Int -> ST s Int
longestWalk c stack nub table accept steps u = do
        writeArray stack u True
        best <- [1..c]
            & map ((u,) >>> (table!))
            & filterM (readArray stack >>> fmap not)
            >>= filterM (\v -> readArray nub v >>= bool (writeArray nub v True >> pure True) (pure False))
            >>= mapM (\v -> writeArray nub v False >> pure v)
            >>= mapM (longestWalk c stack nub table accept (steps+1))
            <&> (bool (minBound:) (steps:) (accept!u) >>> maximum)
        writeArray stack u False
        pure best

parseTable :: Int -> Int -> [C.ByteString] -> UArray (Int,Int) Int
parseTable n c = concatMap (C.words >>> map readInt) >>> listArray ((1,1),(n,c))

parseAccept :: Int -> C.ByteString -> UArray Int Bool
parseAccept n fs = runSTUArray $ do
        accept <- newArray (1,n) False
        mapM_ (readInt >>> flip (writeArray accept) True) (C.words fs)
        pure accept

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
