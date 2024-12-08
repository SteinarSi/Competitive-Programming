module Year.Year2024.Day3 (Day3(Day3)) where

import           Control.Arrow   ((>>>), (***))
import           Data.Char       (isDigit)
import           Data.List.Split (splitOn)
import           Text.Regex.TDFA (getAllTextMatches, (=~))

import           Meta            (AoC (..))
import           Utility.Misc    (read')

data Day3 = Day3
instance AoC Day3 String Int where
    date _ = (3,2024)
    parse _ = id
    part1 _ = mult
    part2 _ = dos >>> mult
    testAnswerPart1 _ = 161
    testAnswerPart2 _ = 48

mult :: String -> Int
mult = (=~ ("mul\\([0-9]+,[0-9]+\\)" :: String))
        >>> getAllTextMatches
        >>> map (drop 4 
            >>> span isDigit 
            >>> read'
                    *** 
                (tail >>> init >>> read')
            >>> uncurry (*))
        >>> sum

dos :: String -> String
dos = splitOn "do()" 
        >>> map (splitOn "don't()" >>> head) 
        >>> concat
