{-# LANGUAGE TupleSections #-}

import           Control.Monad    (filterM)
import           Control.Monad.ST (ST, runST)
import           Data.Array.ST    (STUArray, newArray, readArray, writeArray)
import           Data.Sequence    (Seq (..), fromList)

main :: IO ()
main = do
    [f, s, g, u, d] <- fmap (map read . words) getLine
    putStrLn $ runST $ do
        seen <- newArray (1, f) False
        bfs f g u d (fromList [(s,0)]) seen

bfs :: Int -> Int -> Int -> Int -> Seq (Int,Int) -> STUArray s Int Bool -> ST s String
bfs _ _ _ _ Empty _ = pure "use the stairs"
bfs f g up down ((u,dist) :<| rest) seen
    | u == g = pure (show dist)
    | otherwise = do
        next <- filterM (\(v,_) -> if v > f || v < 1
            then pure False
            else do
                s <- readArray seen v
                writeArray seen v True
                pure (not s)
            )
            (map (,dist+1) [u+up, u-down])
        bfs f g up down (rest <> fromList next) seen
