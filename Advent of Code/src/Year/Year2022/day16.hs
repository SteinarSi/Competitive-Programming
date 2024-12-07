import Data.Array (Array, array, (!), bounds, indices)
import qualified Data.IntSet as S
import qualified Data.Set as SS
import Data.List (nub, sort, delete, sortOn)
import Data.Bifunctor (bimap, first)
import Data.Maybe (fromJust)

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Concurrent (getNumCapabilities, setNumCapabilities, forkIO)
import Control.Concurrent.QSemN (newQSemN, waitQSemN, signalQSemN)
import Control.DeepSeq (deepseq)
import Control.Monad (forM_)

type Flow = Array Int Int
type CliqueGraph = Array (Int, Int) Int

main :: IO ()
main = do
    getNumCapabilities >>= setNumCapabilities
    (g,f) <- fmap (first clique . parse SS.empty [] [] . map words . lines) (readFile "inputs/day16-input.txt")
    let valves = filter ((>0) . (f!)) (indices f)
    print $ pathValue g f 30 0 0 $ last $ sortOn (pathValue g f 30 0 0) (paths g 30 (reverse $ sortOn (f!) valves))
    concurrentBruteForce (S.fromList valves) g f >>= print

concurrentBruteForce :: S.IntSet -> CliqueGraph -> Flow -> IO Int
concurrentBruteForce valves g f = do
    let set = powerSet valves
    best <- newMVar 0
    qsem <- newQSemN 0
    forM_ set (\s -> forkIO $ do
            let res1 = search (S.difference valves s)
                res2 = search s
            deepseq (res1, res2) (modifyMVar_ best (pure . max (res1+res2)))
            signalQSemN qsem 1)
    waitQSemN qsem (SS.size set) 
    readMVar best
    where search :: S.IntSet -> Int
          search = pathValue g f 26 0 0 . last . sortOn (pathValue g f 26 0 0) . paths g 26 . S.toList

pathValue :: CliqueGraph -> Flow -> Int -> Int -> Int -> [Int] -> Int
pathValue g f time current prev [] = time * current
pathValue g f time current prev (u:us) = current * g ! (prev,u) + pathValue g f (time-g!(prev,u)) (current + f ! u) u us

paths :: CliqueGraph -> Int -> [Int] -> [[Int]]
paths g = paths' 0 [] 
    where 
        paths' :: Int -> [Int] -> Int -> [Int] -> [[Int]]
        paths' _    ret _    [] = [reverse ret]
        paths' prev ret time xs | null next = [reverse ret]
                                | otherwise = concatMap (\x -> paths' x (x:ret) (time - g ! (prev,x)) (delete x xs)) next
            where next = filter (\x -> g ! (prev, x) < time) xs

clique :: Array Int [Int] -> CliqueGraph
clique g = array ((0,0), (n, n)) $ concatMap (\i -> bfs i (S.singleton i) [i] [((i,i),0)] 2) (indices g)
    where 
        n = snd (bounds g)
        bfs :: Int -> S.IntSet -> [Int] -> [((Int, Int), Int)] -> Int -> [((Int, Int), Int)]
        bfs start visited gen ret dist | null next = ret
                                       | otherwise = bfs start (S.union visited (S.fromList next)) next (map (\v -> ((start,v),dist)) next ++ ret) (dist+1)
            where next = nub $ filter (flip S.notMember visited) (concatMap (g!) gen) 

parse :: SS.Set String -> [(String, [String])] -> [(String, Int)] -> [[String]] -> (Array Int [Int], Flow)
parse names neighbours flows [] = let aix = array (0, fromIntegral (SS.size names)-1)
                                      translate = fromJust . flip lookup (zip (sort $ SS.toList names) [0..])
                                      neighbours' = map (bimap translate (map translate)) neighbours
                                      flows' = map (first translate) flows
                                  in  (aix neighbours', aix flows')
parse names neighbours flows ((_:name:_:_:flow:_:_:_:_:neighs):xs) = parse (SS.insert name names) ((name,ns):neighbours) ((name,f):flows) xs
    where ns = last neighs : map init (init neighs)
          f  = read (init (drop 5 flow))

powerSet :: S.IntSet -> SS.Set S.IntSet
powerSet = SS.map (S.fromList . SS.toList) . SS.powerSet . SS.fromList . S.toList