import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, forM_)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (UArray, bounds, newArray, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (STUArray, runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq
import           Text.Printf           (printf)

main :: IO ()
main = do
    [n,w,h]:[sx,sy,tx,ty]:bases <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))
    let dist = distances (w,h) bases
    putStrLn (escape dist (sx,sy) (tx,ty))

escape :: UArray (Int,Int) Int -> (Int,Int) -> (Int,Int) -> String
escape dist (sx,sy) (tx,ty) = bfs undefined 0 (min (dist ! (sx,sy)) (dist ! (tx,ty)))
    where
        bfs :: (Int,Int) -> Int -> Int -> String
        bfs b lo hi | lo >= hi = case path hi of
                        Nothing -> printf "%d %d" (lo-1) (fromJust (path (lo-1)))
                        Just  d -> printf "%d %d" hi d
                    | otherwise = case path mi of
                        Nothing -> bfs b lo mi
                        Just  d -> bfs (mi,d) (mi + 1) hi
            where mi = (lo + hi) `div` 2

        path :: Int -> Maybe Int
        path d = runST (newArray (bounds dist) False >>= flip bfs (Seq.singleton (0, (sx,sy))))
            where
                bfs :: STUArray s (Int,Int) Bool -> Seq (Int,(Int,Int)) -> ST s (Maybe Int)
                bfs seen Empty = pure Nothing
                bfs seen ((c,u) :<| xs) | u == (tx,ty) = pure (Just c)
                                        | otherwise    = do
                    ys <- neighbours u
                        & filter (inRange (bounds dist))
                        & filter ((dist!) >>> (>=d))
                        & filterM (\v -> readArray seen v >>= \s -> if s then pure False else writeArray seen v True >> pure True)
                    done <- readArray seen (tx,ty)
                    if done
                        then pure (Just (c+1))
                        else bfs seen (xs <> Seq.fromList (map (c+1,) ys))

distances :: (Int,Int) -> [[Int]] -> UArray (Int,Int) Int
distances (w,h) bases = runSTUArray $ do
        dist <- newArray rng maxBound
        forM_ bases $ \[x,y] -> do
            writeArray dist (x,y) 0
            bfs dist (x,y) (Seq.singleton (x,y))
        pure dist
    where
        rng = ((0,0),(w-1,h-1))

        bfs :: STUArray s (Int,Int) Int -> (Int,Int) -> Seq (Int,Int) -> ST s ()
        bfs _ _ Empty = pure ()
        bfs dist b@(bx,by) (u@(x,y) :<| xs) = do
            d <- readArray dist u
            let nd = abs (x-bx) + abs (y-by)
            neighbours u
                & filter (inRange rng)
                & filterM (\v -> readArray dist v >>= \s -> if s <= nd+1 then pure False else writeArray dist v (nd+1) >> pure True)
                >>= (Seq.fromList >>> (xs<>) >>> bfs dist b)

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
