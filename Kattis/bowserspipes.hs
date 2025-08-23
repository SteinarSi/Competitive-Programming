import           Control.Applicative   (liftA2)
import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (MArray (newArray, newArray_),
                                        STUArray (STUArray), UArray (UArray),
                                        readArray, writeArray)
import           Data.Array.ST         (STUArray, freeze, newArray)
import           Data.Array.Unboxed    (UArray, array, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- fmap readInt C.getLine
    down <- fmap (array (0, n-1) . zip [0..] . map readInt . C.words) C.getLine
    queries <- fmap (map readInt . C.words) (C.getLine >> C.getLine)

    let (luigi, mario) = preprocess n down

    forM_ queries ((luigi !) >>> (mario !) >>> print)


findCoin :: UArray Int Int -> Int -> Int -> (Int, Int)
findCoin down steps u | down ! u == -1 = (steps, u)
                      | otherwise      = findCoin down (succ steps) (down ! u)

preprocess :: Int -> UArray Int Int -> (UArray Int Int, UArray Int Int)
preprocess n down = runST $ do
    isPipe <- newArray (0, n-1) True    :: ST s (STUArray s Int Bool)
    l <- newArray_ (0, n-1)             :: ST s (STUArray s Int Int)
    m <- newArray_ (0, n-1)             :: ST s (STUArray s Int Int)
    steps <- newArray (0, n-1) 99999999 :: ST s (STUArray s Int Int)

    let nonCoins = [ u | u <- [0..n-1], down ! u /= -1]

    forM_ nonCoins $ \u -> writeArray isPipe (down ! u) False

    forM_ nonCoins $ \u -> do
        pipe <- readArray isPipe u
        when pipe $ do
            let (step, coin) = findCoin down 0 u
            writeArray l u coin
            best <- readArray steps coin
            when (step < best) $ do
                writeArray m coin u
                writeArray steps coin step

    liftA2 (,) (freeze l) (freeze m)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt
