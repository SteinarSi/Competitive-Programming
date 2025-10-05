import           Control.Arrow         ((>>>))
import           Control.Monad         (foldM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, (!))
import           Data.Array.ST         (Ix, MArray (..), STUArray, getElems,
                                        readArray, runSTArray, writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,m,k]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let graph = runSTArray $ do
            g <- newArray (1,n) []
            mapM_ (\[a,b] -> modifyArray g a (b:) >> modifyArray g b (a:)) xs
            pure g

    distribute n k graph
        & maybe "impossible" (map show >>> unwords)
        & putStrLn

distribute :: Int -> Int -> Array Int [Int] -> Maybe [Int]
distribute n k graph = runST $ do
    games <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
    valid <- mapM (\u -> readArray games u >>= ((>0) >>> bool (dfs games u 1 <&> (>k)) (pure True))) [1..n]
    if and valid
        then getElems games <&> Just
        else pure Nothing
  where
    dfs :: STUArray s Int Int -> Int -> Int -> ST s Int
    dfs games u g = do
        writeArray games u (min k g)
        foldM (\r v -> readArray games v >>= ((>0) >>> bool (dfs games v r) (pure r))) (g+1) (graph ! u)

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
