import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, unless, when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, getAssocs, newArray,
                                        readArray, writeArray)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> parse
        >>> map (uncurry solve)
        >>> unlines
        >>> putStr
    )

solve :: Int -> [Int] -> String
solve t xs = runST $ do
    best <- newArray (0,3600) (-1)
    writeArray best 0 0
    bfs best 1 [0]
    (d,p) <- getAssocs best <&> (find (\(a,b) -> a >= t && b /= -1) >>> fromJust)
    pure (show p <> " " <> show (d-t))
  where
    bfs :: STUArray s Int Int -> Int -> [Int] -> ST s ()
    bfs _ _ [] = pure ()
    bfs best p q = do
        r <- readArray best t
        unless (r /= -1) $ do
            [ u+x | u <- q, x <- xs ]
                & filter (>0)
                & map (min 3600)
                & filterM (\v -> do
                    s <- readArray best v <&> (== -1)
                    when s (writeArray best v p)
                    pure s)
                >>= bfs best (p+1)

parse :: [[Int]] -> [(Int,[Int])]
parse []             = []
parse ([_,t]:xs:xss) = (t,xs) : parse xss

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
