{-# LANGUAGE StrictData #-}

import           Control.Arrow    ((>>>))
import           Control.Monad    (filterM)
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (MArray (..), STUArray, readArray, writeArray)
import           Data.Char        (chr, ord)
import           Data.Function    ((&))
import           Data.Maybe       (mapMaybe)
import           Data.Sequence    (Seq (..))
import qualified Data.Sequence    as Seq

data State = State !Int !Int !Int !Int !Int !Int !Int !Int
    deriving (Eq)

main :: IO ()
main = do
    start <- getLine
    goal  <- getLine
    print (solve start goal)

solve :: String -> String -> Int
solve start goal = runST $ do
    seen <- newArray (0,6^8) False
    writeArray seen (hash s) True
    bfs seen (Seq.singleton (0,s))
  where
    s = parse start
    g = parse goal

    bfs :: STUArray s Int Bool -> Seq (Int,State) -> ST s Int
    bfs seen Empty = undefined
    bfs seen ((i,x) :<| xs)
        | x == g    = pure i
        | otherwise = do
            moves x
                & filterM (\x -> do
                    let h = hash x
                    s <- readArray seen h
                    if s
                        then pure False
                        else writeArray seen h True >> pure True)
                >>= (map (i+1,) >>> Seq.fromList >>> (xs<>) >>> bfs seen)

moves :: State -> [State]
moves state@(State x1 x2 x3 x4 x5 x6 x7 x8) = mapMaybe move (zip [1..8] [x1,x2,x3,x4,x5,x6,x7,x8])
    where
    move :: (Int,Int) -> Maybe State
    move (x,c) = case (x,c) of
        (1,0)             -> Just (rotateSingle 2 state)
        (8,0)             -> Just (rotateSingle 7 state)
        (_,0)             -> Just (rotateList [x-1,x+1])
        (1,1)             -> Nothing
        (2,1)             -> Just (State x1 x2 x1 x4 x5 x6 x7 x8)
        (3,1)             -> Just (State x1 x2 x3 x2 x5 x6 x7 x8)
        (4,1)             -> Just (State x1 x2 x3 x4 x3 x6 x7 x8)
        (5,1)             -> Just (State x1 x2 x3 x4 x5 x4 x7 x8)
        (6,1)             -> Just (State x1 x2 x3 x4 x5 x6 x5 x8)
        (7,1)             -> Just (State x1 x2 x3 x4 x5 x6 x7 x6)
        (8,1)             -> Nothing
        (_,2)             -> Just (rotateSingle (9-x) state)
        (1,3)             -> Nothing
        (8,3)             -> Nothing
        (_,3) | x <= 4    -> Just (rotateList [1..x-1])
              | otherwise -> Just (rotateList [x+1..8])
        (1,4)             -> Nothing
        (8,4)             -> Nothing
        (_,4)             -> let y = min (8-x) (x-1)
                             in  Just (rotateList [x-y,x+y])
        (_,5) | odd x     -> Just (rotateSingle ((x+9) `div` 2) state)
              | otherwise -> Just (rotateSingle (x `div` 2) state)

    rotateList :: [Int] -> State
    rotateList = foldr rotateSingle state

rotateSingle :: Int -> State -> State
rotateSingle x (State x1 x2 x3 x4 x5 x6 x7 x8) = case x of
    1 -> State (rot x1) x2 x3 x4 x5 x6 x7 x8
    2 -> State x1 (rot x2) x3 x4 x5 x6 x7 x8
    3 -> State x1 x2 (rot x3) x4 x5 x6 x7 x8
    4 -> State x1 x2 x3 (rot x4) x5 x6 x7 x8
    5 -> State x1 x2 x3 x4 (rot x5) x6 x7 x8
    6 -> State x1 x2 x3 x4 x5 (rot x6) x7 x8
    7 -> State x1 x2 x3 x4 x5 x6 (rot x7) x8
    8 -> State x1 x2 x3 x4 x5 x6 x7 (rot x8)

parse :: String -> State
parse xs = State x1 x2 x3 x4 x5 x6 x7 x8
  where
    [x1,x2,x3,x4,x5,x6,x7,x8] = map (ord >>> subtract (ord 'A')) xs

hash :: State -> Int
hash (State x1 x2 x3 x4 x5 x6 x7 x8) = x1 + 6*x2 + 36*x3 + 216*x4 + 1296*x5 + 7776*x6 + 46656*x7 + 279936*x8

rot :: Int -> Int
rot 5 = 0
rot x = x+1
