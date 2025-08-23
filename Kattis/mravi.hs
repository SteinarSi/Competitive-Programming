import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, replicateM, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, array, bounds, freeze,
                                        indices, listArray, newArray, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (STArray)
import           Data.Array.Unboxed    (Array, UArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- C.getLine <&> readInt
    rows <- replicateM (n-1) C.getLine <&> map (C.words >>> map readInt)
    req <- C.getLine <&> (C.words >>> map (readInt >>> fromIntegral) >>> listArray (1,n))
    let (tree, pow) = parse n rows
    print $ solve tree pow req

solve :: Array Int [(Int,Double)] -> UArray Int Bool -> UArray Int Double -> Double
solve tree pow req = arr ! 1
    where
        arr :: Array Int Double
        arr = array (bounds tree) $ do
            i <- indices tree
            let r = if req ! i /= -1
                then req ! i
                else maximum $ map (\(b, d) -> arr ! b / d) (tree ! i)
            pure (i, bool r (sqrt r) (pow ! i))

parse :: Int -> [[Int]] -> (Array Int [(Int,Double)], UArray Int Bool)
parse n rows = runST $ do
    t <- newArray (1,n) []    :: ST s (STArray s Int [(Int,Double)])
    p <- newArray (1,n) False :: ST s (STUArray s Int Bool)
    forM_ rows $ \[a,b,x,e] -> readArray t a >>= (((b, fromIntegral x / 100):) >>> writeArray t a) >> when (e == 1) (writeArray p b True)
    (,) <$> freeze t <*> freeze p

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
