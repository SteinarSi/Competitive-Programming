module Year.Year2022.Day12 (Day12(Day12)) where

import           Control.Monad   (guard)
import           Data.Array.Base (UArray, assocs, bounds, listArray, (!), (//))
import           Data.Char       (isAlpha)
import           Data.Function   ((&))
import           Data.List       (nub)
import qualified Data.Map        as M
import           Data.Maybe      (fromJust)

import           Meta            (AoC (..))

data Day12 = Day12
instance AoC Day12 (UArray (Int, Int) Char, (Int,Int), (Int,Int)) Int where
    parse _ xs = let arr = listArray ((0, 0), (length (lines xs)-1, length (head (lines xs))-1)) (filter isAlpha xs)
                     s = find arr 'S'
                     e = find arr 'E'
                 in  (arr // [(e, 'z'), (s, 'a')], s, e)
    part1 _ (arr,s,e) = search False [s] 0 arr M.empty
            & M.lookup e
            & fromJust
    part2 _ (arr,s,e) = search True  [e] 0 arr M.empty
            & M.assocs
            & filter (\(k, a) -> arr ! k == 'a')
            & map snd
            & minimum
    date _  = 12
    year _  = 2022
    testAnswerPart1 _ = 31
    testAnswerPart2 _ = 29

search :: Bool -> [(Int, Int)] -> Int -> UArray (Int, Int) Char -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
search back [] count map' dist = dist
search back gen count map' dist = search back g (count+1) map' d
    where d = M.union dist (M.fromList (zip gen (repeat count)))
          g = nub (concatMap neighs gen)
          (m,n) = snd (bounds map')
          neighs (y, x) = do
            (a, b) <- [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]
            guard (a /= y || b /= x)
            guard (a >= 0 && b >= 0 && a <= m && b <= n)
            guard (not back && map' ! (y,x) >= pred (map' ! (a,b)) || back && map' ! (a,b) >= pred (map' ! (y,x)))
            guard (M.notMember (a,b) dist && notElem (a,b) gen)
            pure (a, b)    

find :: UArray (Int, Int) Char -> Char -> (Int, Int)
find arr a = fst $ head (filter ((a==) . snd) (assocs arr))
