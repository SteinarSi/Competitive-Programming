import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        runSTUArray, writeArray)
import           Data.Array.Unboxed    (UArray, bounds, inRange, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let rng = ((0,0),(n-1,m-1))
        graph = runSTUArray $ do
            g <- newArray rng True
            mapM_ (\[x,y] -> writeArray g (x,y) False) (init xs)
            pure g
        [tx,ty] = last xs

    putStrLn $ runST $ do
        seen <- newArray rng False
        writeArray seen (0,0) True
        bfs graph seen [(0,0)]
        readArray seen (tx,ty) <&> bool "IMPOSSIBLE" "SLEEPING"

bfs :: UArray (Int,Int) Bool -> STUArray s (Int,Int) Bool -> [(Int,Int)] -> ST s ()
bfs graph seen [] = pure ()
bfs graph seen xs = xs
    & concatMap (\(x,y) -> filter (\v -> inRange (bounds graph) v && graph ! v) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)])
    & filterM (\u -> readArray seen u >>= bool (writeArray seen u True >> pure True) (pure False))
    >>= bfs graph seen

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
