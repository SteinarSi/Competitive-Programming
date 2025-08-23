import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (find, partition)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [g]:[n]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let (r,w) = simulate xs
        (a,b) | r == g    = w
              | otherwise = (r,g)

    putStrLn (show a <> " " <> show b)

simulate :: [[Int]] -> (Int,(Int,Int))
simulate xs = runST $ do
    state <- newArray (1,3) False :: ST s (STUArray s Int Bool)
    writeArray state 2 True
    forM_ xs $ \[x,y] -> do
        a <- readArray state x
        b <- readArray state y
        writeArray state x b
        writeArray state y a
    getAssocs state <&> (partition snd >>> (fst >>> head >>> fst) &&& (snd >>> map fst >>> head &&& last))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
