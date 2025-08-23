import           Control.Arrow               ((***), (>>>))
import           Control.Monad               (filterM, forM_, unless, zipWithM)
import           Control.Monad.ST            (ST, runST)
import qualified Data.ByteString.Char8       as C
import           Data.Function               ((&))
import           Data.Functor                ((<&>))
import           Data.Ix                     (range)
import           Data.Maybe                  (fromJust)
import           Data.STRef.Strict           (STRef, modifySTRef, newSTRef,
                                              readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed.Mutable as V

limit :: Int
limit = 501

main :: IO ()
main = do
    rss <- C.getContents <&> (C.lines
        >>> map (C.words >>> map readInt)
        >>> parse)

    let ans = runST $ do
            seen <- V.replicate (limit^2) 0
            ret <- newSTRef 0
            zipWithM (delulu ret seen) [1..] rss

    mapM_ print ans

delulu :: STRef s Int -> V.STVector s Int -> Int -> [((Int,Int),(Int,Int))] -> ST s Int
delulu ret seen it rs = do
    writeSTRef ret 0
    forM_ rs (range
        >>> mapM_ (\(x,y) -> do
                let i = x + limit*y
                s <- V.read seen i
                unless (s==it) (V.write seen i it >> modifySTRef ret succ)
            )
        )
    readSTRef ret

parse :: [[Int]] -> [[((Int,Int),(Int,Int))]]
parse [] = []
parse [_] = []
parse ([n]:xs) = splitAt n xs
    & map (\(a:b:c:d:_) -> ((a, b), (c-1, d-1))) *** parse
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
