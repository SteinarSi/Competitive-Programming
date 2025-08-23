import           Control.Monad         (replicateM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray (STUArray))
import           Data.Array.ST         (STUArray, getElems, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t <- fmap readInt B.getLine
    replicateM_ t $ do
        n <- fmap readInt B.getLine
        xs <- fmap (map readInt . B.words) B.getLine
        m <- fmap readInt B.getLine
        ys <- fmap (map readInt . B.words) B.getLine
        let a = runST $ do
                coeff <- newArray (0,n+m) 0 :: ST s (STUArray s Int Int)
                sequence_ $ do
                    (i,x) <- zip [0..] xs
                    (j,y) <- zip [0..] ys
                    pure (readArray coeff (i+j) >>= writeArray coeff (i+j) . (x*y+))
                getElems coeff
        print (n+m)
        putStrLn (unwords (map show a))

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt
