import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Int (Int8, Int16, Int32)
import Control.Monad.ST
import Data.Array.ST

main = do
    [v, e] <- fmap (map read . words) getLine
    edges <- fmap (map words) $ replicateM e getLine
    let neighbours = 
    --let neighbours = foldl' (\m [u, v, w] -> M.insertWith S.union (read u) (S.singleton (read v, read w)) m) M.empty edges :: M.Map Int16 (S.Set (Int32, Int8))
    [s, t] <- fmap (map read . words) getLine ::IO [Int16]

    putStrLn "hello, placeholder"

--createNeigbours :: [[String]] -> ST s (STArray s Int16 [(Int16, Int8)])


{-
 procedure Shortest-Path-Faster-Algorithm(G, s)
  1    for each vertex v ≠ s in V(G)
  2        d(v) := ∞
  3    d(s) := 0
  4    push s into Q
  5    while Q is not empty do
  6        u := poll Q
  7        for each edge (u, v) in E(G) do
  8            if d(u) + w(u, v) < d(v) then
  9                d(v) := d(u) + w(u, v)
 10                if v is not in Q then
 11                    push v into Q
-}