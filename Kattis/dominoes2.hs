import           Control.Arrow         (second, (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array, Ix, bounds, (!))
import           Data.Array.ST         (MArray, STUArray, newArray, readArray,
                                        runSTArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (uncurry solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: Array Int [Int] -> [Int] -> Int
solve graph qs = runST $ do
    standing <- newArray (bounds graph) True
    mapM (fell standing) qs <&> sum
  where
    fell :: STUArray s Int Bool -> Int -> ST s Int
    fell standing u = do
        f <- readArray standing u
        if f
            then writeArray standing u False >> mapM (fell standing) (graph ! u) <&> (sum >>> succ)
            else pure 0

parse :: [[Int]] -> [(Array Int [Int], [Int])]
parse [] = []
parse ([n,m,l]:xs) = (graph,map head qs) : parse rest
  where
    (edges,(qs,rest)) = second (splitAt l) (splitAt m xs)
    graph = runSTArray $ do
        g <- newArray (1,n) []
        mapM_ (\[x,y] -> modifyArray g x (y:)) edges
        pure g

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr ix f = readArray arr ix >>= (f >>> writeArray arr ix)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
