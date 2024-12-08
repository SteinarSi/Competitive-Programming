module Year.Year2023.Day25(Day25(Day25), parse25) where

import           Control.Monad        (forM_, liftM2)
import           Data.List            (nub)

import           Data.Bifunctor       (bimap)
import           Data.IntSet          (IntSet)
import qualified Data.IntSet          as IS
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import           System.Random        (RandomGen (next), StdGen)


import           Control.Monad.ST     (runST)
import           Data.Maybe           (fromJust)
import           Meta                 (AoC (..))
import           Utility.Misc

data Day25 = Day25
instance AoC Day25 (Edges, Graph) Int where
    date _ = (25,2023)
    parse _ = buildGraph M.empty . map words . lines . filter (/=':')
    part1 _ = const 0
    part2 _ = const 49
    testAnswerPart1 _ = 54
    testAnswerPart2 _ = 49

type Graph = Map Int IntSet
type Edges = Set (Int,Int)

buildGraph :: Map String Int -> [[String]] -> (Edges, Graph)
buildGraph ids []           = (S.empty, M.empty)
buildGraph ids ([_]:xss) = buildGraph ids xss
buildGraph ids ((x:y:xs):xss) = liftM2 bimap S.insert insertEdge (xid,yid) (buildGraph ids'' ((x:xs):xss))
    where
        (ids',  xid) = emplace x ids
        (ids'', yid) = emplace y ids'
buildGraph _ _ = error "bruh"

insertEdge :: (Int, Int) -> Graph -> Graph
insertEdge (from,to) graph = M.insertWith IS.union from (IS.singleton from) (M.insertWith IS.union to IS.empty graph)

emplace :: Ord k => k -> Map k Int -> (Map k Int, Int)
emplace k m = (m', fromJust (M.lookup k m'))
    where m' = M.insertWith const k (M.size m) m

karger :: StdGen -> (Edges, Graph) -> Int
karger gen (edges, graph) = runST $ do
    pure 42

    -- where
    --     karginTime :: Edges -> Graph ->













parse25 :: IO ()
parse25 = do
    xss <- fmap (map words . lines) getContents
    print $ length $ nub (concatMap (\(x:xs) -> init x : xs) xss)
    forM_ xss $ \(x:xs) -> putStrLn (unwords ([init x, show (length xs)] ++ xs))
