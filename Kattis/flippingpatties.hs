import           Control.Arrow               ((&&&), (>>>))
import           Control.Monad               (forM)
import           Control.Monad.ST            (ST, runST)
import           Data.Bool                   (bool)
import qualified Data.ByteString.Char8       as C
import           Data.Functor                ((<&>))
import           Data.Maybe                  (catMaybes, fromJust)
import qualified Data.Vector.Unboxed.Mutable as V

main :: IO ()
main = do
    xs <- C.getContents <&> (C.lines
        >>> tail
        >>> map (C.words
            >>> map readInt
            >>> (head &&& last)))

    print $ runST (V.replicate 43201 0 >>= flipPatties [] xs)

flipPatties :: [Int] -> [(Int,Int)] -> V.STVector s Int -> ST s Int
flipPatties ys []         count = mapM (V.read count) ys <&> (maximum >>> succ >>> (`div` 2))
flipPatties ys ((d,t):xs) count = do
    ys' <- forM [t-d-d,t-d,t] $ \i -> do
        c <- V.read count i
        V.write count i (c+1)
        pure (bool Nothing (Just i) (c==0))
    flipPatties (catMaybes ys' ++ ys) xs count

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
