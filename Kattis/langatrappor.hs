import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed (UArray, listArray, (!))
import           Data.Function      (on, (&))
import           Data.Functor       ((<&>))
import           Data.List          (minimumBy)
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S

main :: IO ()
main = do
    [[r,c],cs,[sx,sy],[gx,gy]] <- getContents <&> (lines >>> map (words >>> map read))

    let
        houses :: UArray Int Int
        houses = listArray (0,c-1) cs

        height :: (Int,Int) -> Int
        height (x,y) = houses ! x - y

        graph :: M.Map (Int,Int) [((Int,Int),Int)]
        graph = M.fromList [((x,y),edges (x,y)) | x <- [0..c-1], y <- [sy,gy,r-1]]
          where
            edges (x,y) = [
                (((x-1,y), height (x-1,y)), x > 0),
                (((x+1,y), height (x+1,y)), x < c-1),
                (((x,r-1), sumFromTo (height (x,r-1)) (height (x,y+1))), y /= r-1),
                (((x,gy), sumFromTo (height (x,y+1)) (height (x,gy))), y < gy),
                (((x,gy), sumFromTo (height (x,gy)) (height (x,y-1))), y > gy)
                ]
                & filter snd
                & map fst

        dijkstra :: S.Set (Int,Int) -> M.Map (Int,Int) Int -> Int
        dijkstra searched dist
            | null unsearched = dist M.! (gx,gy)
            | otherwise       = graph M.! u
                & map (fmap (du+))
                & foldr (uncurry (M.insertWith min)) dist
                & dijkstra (S.insert u searched)
          where
            unsearched = M.assocs dist
                & filter (fst >>> (`S.notMember` searched))
            (u,du) = minimumBy (compare `on` snd) unsearched

    print (dijkstra S.empty (M.singleton (sx,sy) (height (sx,sy))))

sumFromTo :: Int -> Int -> Int
sumFromTo a b
    | b < a = sumFromTo b a
    | otherwise = (b*(b+1)) `div` 2 - (a*(a-1)) `div` 2
