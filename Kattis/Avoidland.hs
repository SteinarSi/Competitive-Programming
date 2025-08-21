import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (forM_, liftM2)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ([n],xss) <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt) >>> head &&& tail)

    let (xs,ys) = runST $ do
            xx <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
            yy <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
            forM_ xss $ \[x,y] -> do
                readArray xx x >>= (succ >>> writeArray xx x)
                readArray yy y >>= (succ >>> writeArray yy y)
            liftM2 (,) (getElems xx) (getElems yy)

    print (spread 0 xs + spread 0 ys)

spread :: Int -> [Int] -> Int
spread curr []     = abs curr
spread curr (x:xs) = abs curr + spread (curr+x-1) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
