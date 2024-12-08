{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [r,d] <- getContents <&> (words >>> map read)

    let points = if
            | r == 1 && d == 1 -> []
            | r == 1 || d == 1 -> [(0,0),(r-1,d-1),(0,0)]
            | r == 2           -> [(0,0),(0,d-1),(1,d-1),(1,0),(0,0)]
            | d == 2           -> [(0,0),(r-1,0),(r-1,1),(0,1),(0,0)]
            | even r           -> [(0,0)] 
                            <> concatMap (\i -> [(i-1,1),(i-1,d-1),(i,d-1),(i,1)]) [1,3..r]
                            <> [(r-1,0),(0,0)]
            | even d           -> [(0,0)] 
                            <> concatMap (\j -> [(1,j-1),(r-1,j-1),(r-1,j),(1,j)]) [1,3..d] 
                            <> [(0,d-1),(0,0)]
            | r == 3           -> [(0,0),(2,0),(2,d-1),(0,d-1)] 
                            <> concatMap (\i -> [(0,i),(1,i),(1,i-1),(0,i-1)]) [d-2,d-4..3] 
                            <> [(0,1),(1,1),(0,1),(0,0)]
            | otherwise        -> [(0,0),(r-1,0)] 
                            <> concatMap (\i -> [(i+1,d-1),(i,d-1),(i,1),(i-1,1)]) [r-2,r-4..3]
                            <> [(2,d-1),(0,d-1)] 
                            <> concatMap (\i -> [(0,i),(1,i),(1,i-1),(0,i-1)]) [d-2,d-4..3] 
                            <> [(0,1),(1,1),(0,1),(0,0)]
    C.putStr (format ((0,0) : waypoints points))

format :: [(Int,Int)] -> C.ByteString
format = map (\(a,b) -> C.pack (show a <> " " <> show b)) >>> C.unlines

waypoints :: [(Int,Int)] -> [(Int,Int)]
waypoints [] = []
waypoints [_] = []
waypoints ((x,y):(a,b):xs) = line <> waypoints ((a,b):xs)
    where
        line :: [(Int,Int)]
        line | dx /= 0   = drop 1 [ (x',y) | x' <- [x,x+dx..a] ]
             | dy /= 0   = drop 1 [ (x,y') | y' <- [y,y+dy..b] ]
             | otherwise = error ("bruh: " <> show (x,y) <> ", " <> show (a,b))

        dx = signum (a-x)
        dy = signum (b-y)
