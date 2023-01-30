import qualified Data.Array as A
import qualified Data.Map   as M
import Data.Array ((!), (//))
import Data.List (nub)
import Data.Char (isAlpha)
import Data.Maybe (fromJust)
import Control.Monad (guard)

main :: IO ()
main = do
    arr <- fmap toArray (readFile "inputs/day12-input.txt")
    let s = find arr 'S'
        e = find arr 'E'
        there     = search False [s] 0 (arr // [(e, 'z'), (s, 'a')]) M.empty
        backAgain = search True  [e] 0 (arr // [(e, 'z'), (s, 'a')]) M.empty
    print (fromJust $ M.lookup e there, minimum $ map snd $ filter (\(k, a) -> arr ! k == 'a') $ M.assocs backAgain)

toArray :: String -> A.Array (Int, Int) Char
toArray xs = A.listArray ((0, 0), (length (lines xs)-1, length (head (lines xs))-1)) (filter isAlpha xs)

search :: Bool -> [(Int, Int)] -> Int -> A.Array (Int, Int) Char -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
search back [] count map' dist = dist
search back gen count map' dist = search back g (count+1) map' d
    where d = M.union dist (M.fromList (zip gen (repeat count)))
          g = nub (concatMap neighs gen)
          (m,n) = snd (A.bounds map')
          neighs (y, x) = do
            (a, b) <- [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]
            guard (a /= y || b /= x)
            guard (a >= 0 && b >= 0 && a <= m && b <= n)
            guard (not back && map' ! (y,x) >= pred (map' ! (a,b)) || back && map' ! (a,b) >= pred (map' ! (y,x))) -- elevation constraints
            guard (M.notMember (a,b) dist && notElem (a,b) gen)
            pure (a, b)    

find :: Eq a => A.Array (Int, Int) a -> a -> (Int, Int)
find arr a = fst $ head (filter ((a==) . snd) (A.assocs arr))