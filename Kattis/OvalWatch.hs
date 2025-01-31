import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (array)
import           Data.Array.Base       (STUArray, elems, getAssocs,
                                        newListArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)

main :: IO ()
main = do
    (n,k):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    putStrLn $ runST $ do
        arr <- newListArray (0,n-1) [0..n-1] :: ST s (STUArray s Int Int)
        sortOn snd xs
            & mapM_ (\(a,_) -> do
                x <- readArray arr a
                y <- readArray arr (a+1)
                writeArray arr (a+1) x
                writeArray arr a y)
        getAssocs arr <&> (map swap >>> array (0,n-1) >>> elems >>> map show >>> unwords)

trace :: Int -> [Int] -> Int
trace p [] = p
trace p (x:xs) | p == x    = trace (x+1) xs
               | p == x+1  = trace x xs
               | otherwise = trace p xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
