{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (bimap)
import Data.List      (nub)
import Data.Functor   ((<&>))
import Control.Arrow  ((>>>))

data State = Folded | Fist | Palm

main :: IO ()
main = do
    [n,p] <- getLine <&> (words >>> map read)

    print $ play n (map (,Folded) [1..p], 0)

play :: Int -> ([(Int,State)], Int) -> Int
play n (hands, i) = case nub (map fst hands) of
        []  -> error "bruh"
        [p] -> p
        _   -> play n (splat hands ((i+n-1) `mod` length hands))
    where
        splat :: [(Int,State)] -> Int -> ([(Int,State)], Int)
        splat ((p,s):xs) 0 = case s of
            Folded -> ((p,Fist):(p,Fist):xs, 0)
            Fist   -> ((p,Palm):xs, 1)
            Palm   -> (xs, 0)
        splat (ps:xs) i = bimap (ps:) succ (splat xs (i-1))
        splat [] _ = error "bruh"
