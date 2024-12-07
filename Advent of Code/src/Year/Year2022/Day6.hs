module Year.Year2022.Day6 (Day6(Day6)) where

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord, chr)
import           Data.Function         ((&))
import qualified Data.IntMap.Strict    as M

import           Meta  (AoC (..))

data Day6 = Day6
instance AoC Day6 C.ByteString Int where
    parse _ = C.pack
    part1 _ = marker 4
    part2 _ = marker 14
    date _  = 6
    year _  = 2022
    testAnswerPart1 _ = 7
    testAnswerPart2 _ = 19

marker :: Int -> C.ByteString -> Int
marker n xs = C.take n xs
        & C.unpack
        & map (ord >>> (,1))
        & M.fromListWith (+)
        & marker' n
    where
        marker' :: Int -> M.IntMap Int -> Int
        marker' i seen | M.size seen == n = i
                       | i >= C.length xs = error "No marker found?"
                       | otherwise = marker' (i+1) seen'
            where 
                x = ord (C.index xs (i-n))
                y = ord (C.index xs i)
                seen' = M.insertWith (+) y 1 $ case M.lookup x seen of
                    Nothing -> error ("Why is " <> [chr x] <> " not in the map?")
                    Just  1 -> M.delete x seen
                    Just  c -> M.insert x (c-1) seen
