import           Control.Applicative   (liftA2)
import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, listArray, newArray_,
                                        readArray, writeArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Ix               (range)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    hw:rest <- C.getContents <&> C.lines

    let [h,w] = map readInt (C.words hw)
        grid  = listArray ((0,0),(h-1,w-1)) (concatMap (C.unpack >>> map (=='X')) rest)

    print (solve h w grid)

solve :: Int -> Int -> UArray (Int,Int) Bool -> Double
solve h w grid = runST $ do
        dp <- newArray_ rng
        mapM_ (\ix -> f dp ix >>= writeArray dp ix) (range rng)
        readArray dp (r,c)
    where
        r = 2*h
        c = 2*w
        rng = ((0,0),(r,c))

        f :: STUArray s (Int,Int) Double -> (Int,Int) -> ST s Double
        f dp (y,0) = pure (5 * fromIntegral y)
        f dp (0,x) = pure (5 * fromIntegral x)
        f dp (y,x)
            | odd y && odd x = pure 99999999999
            | grid ! ((y-1)`div` 2, (x-1) `div` 2) = liftA2 (min >>> (>>> (+5))) (readArray dp (y,x-1)) (readArray dp (y-1,x))
            | otherwise = sequence [
                        readArray dp (y,x-1) <&> (+5),
                        readArray dp (y-1,x) <&> (+5),
                        readArray dp (y-1,x-1) <&> (+(5*pi/2))
                    ] <&> minimum

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>>  fst
