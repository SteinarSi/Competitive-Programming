module Year.Year2022.Day5 (Day5(Day5)) where

import           Control.Arrow         ((>>>), (***))
import           Data.Bool             (bool)
import           Data.Function         ((&))
import qualified Data.IntMap.Strict as M
import           Data.List             (transpose)
import           Data.List.Split       (chunksOf)
import           Data.Maybe            (fromJust, mapMaybe)

import           Meta  (AoC (..))

type Crates = M.IntMap String
type Instruction = (Int,Int,Int)

data Day5 = Day5
instance AoC Day5 (Crates, [Instruction]) String where
    parse _ = lines
            >>> span (null >>> not)
            >>> (init >>> parseCrates) 
                    *** 
                (tail >>> map (words >>> parseInstruction))
    part1 _ = uncurry (simul False)
    part2 _ = uncurry (simul True)
    date _  = 5
    year _  = 2022
    testAnswerPart1 _ = "CMZ"
    testAnswerPart2 _ = "MCD"

simul :: Bool -> M.IntMap String -> [Instruction] -> String
simul multi crates [] = map head (M.elems crates)
simul multi crates ((amount,from,to):xs) = simul multi crates' xs
    where 
        cs = M.lookup from crates
                & fromJust
                & take amount
                & bool reverse id multi
        crates' = crates
                & M.alter (fmap (drop amount)) from
                & M.alter (fmap (cs <>)) to 

parseCrates :: [String] -> Crates
parseCrates = map ((' ':) >>> chunksOf 4) 
        >>> transpose 
        >>> map (mapMaybe parseCrate)
        >>> zip [1..]
        >>> M.fromList
    where
        parseCrate :: String -> Maybe Char
        parseCrate [' ', '[',c,']'] = Just c
        parseCrate _                = Nothing

parseInstruction :: [String] -> Instruction
parseInstruction (_:amount:_:from:_:to:_) = (read amount, read from, read to)
