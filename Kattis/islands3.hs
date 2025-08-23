import           Control.Monad      (filterM)
import           Control.Monad.ST   (ST, runST)
import           Data.Array.ST      (STUArray, newArray, readArray, writeArray)
import           Data.Array.Unboxed (UArray, bounds, array, (!))
import           Data.Ix            (inRange)

main :: IO ()
main = do
    [rows, cols] <- fmap (map read . words) getLine
    coords <- fmap (zip [(x,y) | y <- [0..rows-1], x <- [0..cols-1]] . concat . lines) getContents
    let graph    = array ((0,0),(cols-1,rows-1)) coords :: UArray (Int,Int) Char
        land     = map fst $ filter (('L'==) . snd) coords

    print $ runST $ do
        visited <- newArray (bounds graph) False
        length <$> filterM (dfs graph visited) land

dfs :: UArray (Int,Int) Char ->  STUArray s (Int,Int) Bool -> (Int,Int) -> ST s Bool
dfs graph visited u = do
    vis <- readArray visited u
    if vis || graph ! u == 'W'
        then pure False
        else do
            writeArray visited u True
            mapM_ (dfs graph visited) $ filter (inRange (bounds graph)) $ map (u+++) [(-1,0), (1, 0), (0, -1), (0, 1)]
            pure True

(+++) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(+++) (a,b) (x,y) = (a+x, b+y)
