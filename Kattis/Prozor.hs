{-# LANGUAGE TupleSections #-}

import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, replicateM)
import           Data.Array            (Array, array, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.List             (maximumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [m, n, k] <- fmap (C.words >>> map (C.readInt >>> fromJust >>> fst)) C.getLine
    window <- replicateM m C.getLine <&> (
                map (C.unpack >>> map ((=='*') >>> bool 0 1))
            >>> (\xss -> [ ((x,y),c) | (y,xs) <- zip [1..m] xss, (x,c) <- zip [1..n] xs ])
            >>> array ((1,1),(n,m))
        )
    let (hits, (x,y)) = solve m n k window
    print hits
    format m n k window (x,y)

format :: Int -> Int -> Int -> Array (Int,Int) Int -> (Int,Int) -> IO ()
format m n k grid (x,y) = forM_ [1..m] $ \y -> C.putStrLn (map (\x -> format (x,y) (grid!(x,y))) [1..n] & C.pack)
    where
        format :: (Int,Int) -> Int -> Char
        format (p,q) _ | (p,q) `elem` [(x,y),(x+k-1,y),(x,y+k-1),(x+k-1,y+k-1)] = '+'
                       | p `elem` [x,x+k-1] && q > y && q < y+k-1 = '|'
                       | q `elem` [y,y+k-1] && p > x && p < x+k-1 = '-'
        format _ 1     = '*'
        format _ _     = '.'

solve :: Int -> Int -> Int -> Array (Int,Int) Int -> (Int,(Int,Int))
solve m n k grid = (sum [ grid ! (x,y) | x <- [2..k-1], y <- [2..k-1]],(1,1))
                 & iterate down
                 & take (m-k+1)
                 & concatMap (iterate right >>> take (n-k+1))
                 & maximumBy (compare `on` (\(h,(x,y)) -> (h,-y,-x)))
    where
        right :: (Int,(Int,Int)) -> (Int,(Int,Int))
        right (hits,(x,y)) = (hits+gain-loss,(x+1,y))
            where loss = sum $ map ((x+1,)   >>> (grid!)) [y+1..y+k-2]
                  gain = sum $ map ((x+k-1,) >>> (grid!)) [y+1..y+k-2]

        down  :: (Int,(Int,Int)) -> (Int,(Int,Int))
        down  (hits,(x,y)) = (hits+gain-loss,(x,y+1))
            where loss = sum $ map ((,y+1)   >>> (grid!)) [x+1..x+k-2]
                  gain = sum $ map ((,y+k-1) >>> (grid!)) [x+1..x+k-2]
