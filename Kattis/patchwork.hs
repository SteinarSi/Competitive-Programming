import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array)
import           Data.Array.Base       (UArray, assocs, elems, freeze,
                                        listArray, listUArrayST, newArray,
                                        readArray, writeArray, (!))
import           Data.Array.ST         (STUArray)
import           Data.Array.Unboxed    (UArray, inRange)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isAlpha)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Debug.Trace           (traceShow)

main :: IO ()
main = do
    [r, c] <- C.getLine <&> (C.words >>> map readInt)
    n <- C.getLine <&> readInt
    ps <- replicateM n $ do
        [rp,cp] <- C.getLine <&> (C.words >>> map readInt)
        replicateM rp C.getLine <&> (map C.unpack >>> concat >>> listArray ((0,0),(rp-1,cp-1)))
    let patches = listArray (1,n) ps :: Array Int (UArray (Int,Int) Char)

    sews <- C.getContents <&> (C.lines
            >>> tail
            >>> map (C.words >>> map readInt >>> (\(a:b:c:_) -> (a,b,c)))
        )

    runST (newArray ((0,0),(r-1,c-1)) '.' >>= sew r c patches sews)
        & elems
        & chunksOf c
        & mapM_ putStrLn

sew :: Int -> Int -> Array Int (UArray (Int,Int) Char) -> [(Int,Int,Int)] -> STUArray s (Int,Int) Char -> ST s (UArray (Int,Int) Char)
sew r c patches [] quilt = freeze quilt
sew r c patches ((q,t,p):xs) quilt = do
        assocs (patches ! p)
            & map (\((a,b),c) -> ((a+q,b+t),c))
            & filter (fst >>> inRange ((0,0),(r-1,c-1)))
            & mapM_ (uncurry (writeArray quilt))
        sew r c patches xs quilt

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)
