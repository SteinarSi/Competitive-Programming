import           Control.Arrow         (first, (>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STArray, STUArray, modifyArray,
                                        newArray, readArray, runSTArray,
                                        runSTUArray, writeArray)
import           Data.Array.Unboxed    (Array, UArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = do
    [n,m,t]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let
        (edges,[[s,g],_:spiders]) = splitAt m xs

        graph :: Array Int [(Int,Int)]
        graph = runSTArray $ do
            ret <- newArray (0,n-1) []
            mapM_ (\[u,v,d] -> do
                modifyArray ret u ((d,v):)
                modifyArray ret v ((d,u):)
                ) edges
            pure ret

        fear :: UArray Int Int
        fear = runSTUArray $ do
            ret <- newArray (0,n-1) maxBound
            mapM_ (flip (writeArray ret) 0) spiders
            spiderDijkstra ret (S.fromList (map (0,) spiders))
            pure ret
          where
            spiderDijkstra :: STUArray s Int Int -> S.Set (Int,Int) -> ST s ()
            spiderDijkstra dist queue = case S.minView queue of
                Nothing -> pure ()
                Just ((d,u),queue') -> do
                    du <- readArray dist u
                    if du < d
                        then spiderDijkstra dist queue'
                        else graph ! u
                            & map (first (d+))
                            & filterM (\(dv,v) -> readArray dist v >>= \dv' -> if dv' > dv then writeArray dist v dv >> pure True else pure False)
                            >>= (foldr S.insert queue' >>> spiderDijkstra dist)

        anthonyDijkstra :: STArray s Int (Int,Int) -> S.Set ((Int,Int),Int) -> ST s Int
        anthonyDijkstra dist queue = case S.maxView queue of
            Nothing -> error "bruh"
            Just (((f,d),u),queue')
                | u == g    -> pure f
                | otherwise -> do
                    du <- readArray dist u
                    if du < (f,d)
                        then anthonyDijkstra dist queue'
                        else graph ! u
                            & map (\(w,v) -> ((min f (fear ! v), d-w),v))
                            & filter (fst >>> snd >>> negate >>> (<=t))
                            & filterM (\(dv,v) -> readArray dist v >>= \dv' -> if dv' < dv then writeArray dist v dv >> pure True else pure False)
                            >>= (foldr S.insert queue' >>> anthonyDijkstra dist)

    print $ runST $ do
        dist <- newArray (0,n-1) (minBound,minBound)
        writeArray dist s (fear ! s, 0)
        anthonyDijkstra dist (S.singleton ((fear ! s,0),s))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
