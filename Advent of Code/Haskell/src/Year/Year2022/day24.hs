{-# LANGUAGE Strict #-}

import Data.Bool (bool)
import Data.Set (Set, fromList, toList, member, notMember, empty, union, singleton)
import qualified Data.Set as S
import Data.Maybe (catMaybes)

import Debug.Trace (trace)

main = do
    (weather, to, width, height) <- fmap (parse empty 1 . tail . lines) (readFile "inputs/day24-input.txt")
    print (bfs 0 (singleton (1,0)) (singleton (1,0)) width height (weatherReport weather) to)

type Pos = (Int, Int)
data Blizzard = Blizzard {
        pos  :: Pos,
        from :: Pos,
        to   :: Pos,
        dir  :: Pos
    } deriving (Ord, Eq)

parse :: Set Blizzard -> Int -> [String] -> (Set Blizzard, Pos, Int, Int)
parse weather i [row] = (weather, (length row - 2, i), length row, i+1)
parse weather i (row:xs) = parse (union weather $ fromList $ catMaybes $ map (uncurry blizz) $ zip [0..] row) (i+1) xs
    where blizz _ '.' = Nothing
          blizz _ '#' = Nothing
          blizz x '<' = Just (Blizzard (x, i) (length row - 2, i) (1, i) (-1,0))
          blizz x '>' = Just (Blizzard (x, i) (1, i) (length row - 2, i) (1, 0))
          blizz x '^' = Just (Blizzard (x, i) (x, i + length xs - 1) (x, 1) (0, -1))
          blizz x 'v' = Just (Blizzard (x, i) (x, 1) (x, i + length xs - 1) (0, 1))

weatherReport :: Set Blizzard -> Set Blizzard
weatherReport = fromList . map (\b -> bool (b { pos = pos b +++ dir b }) (b { pos = from b}) (pos b == to b)) . toList

bfs :: Int -> Set Pos -> Set Pos -> Int -> Int -> Set Blizzard -> Pos -> Int
bfs i gen visited width height weather goal | member goal gen = i
                                            | otherwise = trace (show (i, gen, weather) ++ "\n\n") $ bfs (i+1) nextGen (union visited news) width height (weatherReport weather) goal
    where nextGen = union waiters news
          waiters = S.filter (any (flip notMember visited) . flip map dirs . (+++)) gen
          news    = S.fromList $ filter (\p@(x,y) -> x>0 && y > 0 && x < width && y < height && notMember p dangerSpots && notMember p visited && p /= (1, -1)) $ concatMap (flip map dirs . (+++)) gen
          dangerSpots = S.map pos weather
dirs :: [Pos]
dirs = [(0,-1),(0,1),(-1,0),(1,0)]

instance Show Blizzard where
    show b = show (pos b)

(+++) :: Num a => (a, a) -> (a, a) -> (a, a)
(+++) (x,y) (u,v) = (x+u, y+v)

{-
BFS gjennom dalen. Husk på alle posisjoner man kan være på etter k minutt.
Husk at man må kunne vente, kanskje vente 10+ turer på ett sted.
Når du allerede har besøkt alle 4 posisjoner rundt et sted trenger vi ikke lenger vente der.
-}