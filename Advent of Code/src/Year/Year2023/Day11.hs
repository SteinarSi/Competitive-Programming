module Year.Year2023.Day11 where

import           Data.List    (transpose)
import           Meta         (AoC (..))
import           Utility.Misc

data Day11 = Day11
instance AoC Day11 (Integer, [Integer], [Integer]) Integer where
    parse _ s = (sum rs, rs, cs)
        where ls = lines s
              rs = galaxiesPerRow ls
              cs = galaxiesPerCol ls
    part1 _ = distances 1
    part2 _ = distances 999999
    date _ = 11
    year _ = 2023
    testAnswerPart1 _ = 374
    testAnswerPart2 _ = 82000210

distances :: Integer -> (Integer, [Integer], [Integer]) -> Integer
distances expansion (stars, rows, cols) = uncurry (+) $ fmap (expansion*) (rowDist rows +++ rowDist cols)
    where
        rowDist :: [Integer] -> (Integer, Integer)
        rowDist = dist' (1-stars) 0 0 0 . zip [0..]

        dist' :: Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)] -> (Integer, Integer)
        dist' _ _ d coeff []         = (d, coeff)
        dist' weight seen d coeff ((pos,c):xs) = dist' weight' seen' d' coeff' xs
            where
                d' = d + sum (map (pos*) [weight, weight+2.. weight'-2])
                weight' = weight + 2 * c
                seen' = seen + c
                coeff' | c == 0    = coeff + seen * (stars - seen')
                       | otherwise = coeff

galaxiesPerRow :: [String] -> [Integer]
galaxiesPerRow = map (length' . filter ('#' ==))

galaxiesPerCol :: [String] -> [Integer]
galaxiesPerCol = galaxiesPerRow . transpose
