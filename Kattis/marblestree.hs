import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (Array)
import           Data.Array.Base       (STUArray, UArray, array, bounds, elems,
                                        getAssocs, newArray, writeArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find, foldl')
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (solve >>> show)
        >>> unlines
        >>> putStr
    )

solve :: (Int, UArray Int Int, Array Int [Int]) -> Int
solve (root, marbles, graph) = fst (solulu root)
    where
        solulu :: Int -> (Int,Int)
        solulu u = let (cost,count) = graph ! u
                            & map solulu
                            & foldl' (\(a,c) (a',c') -> (a+a',c+c')) (0,marbles ! u - 1)
                    in  (cost + abs count, count)

parse :: [[Int]] -> [(Int, UArray Int Int, Array Int [Int])]
parse [] = []
parse ([n]:xs) = (root, marbles, graph) : parse b
    where
        (a,b) = splitAt n xs
        marbles = array (1,n) (map (\(u:m:_) -> (u,m)) a)
        graph = array (1,n) (map (\(u:_:_:vs) -> (u,vs)) a)

        -- Would be nice if the root was constant, or given in the input, but nope
        -- This is why we can't have nice code sometimes
        root = runST $ do
            r <- newArray (1,n) True :: ST s (STUArray s Int Bool)
            mapM_ (mapM_ (flip (writeArray r) False)) (elems graph)
            getAssocs r <&> (find snd >>> fromJust >>> fst)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
