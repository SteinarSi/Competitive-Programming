module Year.Year2022.Day7 (Day7(Day7)) where

import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import qualified Data.Map   as M

import           Meta          (AoC (..))

type Directory = M.Map [String] [Either Integer String]

data Day7 = Day7
instance AoC Day7 Directory Integer where
    date _ = (7,2022)
    parse _ = lines >>> map words >>> parseDirectory M.empty []
    part1 _ dic = directory (const 0) (\d p -> (+bool 0 (size d p) (size d p <= 100000)) . sum) dic ["/"]
    part2 _ dic = minimum $ filter (>=max 0 (size dic ["/"] - 40000000)) (directory (const []) (\d p -> (size d p:) . concat) dic ["/"])
    testAnswerPart1 _ = 95437
    testAnswerPart2 _ = 24933642

parseDirectory :: Directory -> [String] -> [[String]] -> Directory
parseDirectory dic _ [] = dic
parseDirectory dic path (["$", "cd", ".."]:xs) = parseDirectory dic (tail path) xs
parseDirectory dic path (["$", "cd", name]:xs) = parseDirectory (M.insert (name:path) [] dic) (name:path) xs
parseDirectory dic path (["$", "ls"]:xs)       = parseDirectory dic path xs
parseDirectory dic path (["dir", name]:xs)     = parseDirectory (M.adjust (Right name:) path dic) path xs
parseDirectory dic path ([size, _]:xs)         = parseDirectory (M.adjust (Left (read size):) path dic) path xs

directory :: (Integer -> a) -> (Directory -> [String] -> [a] -> a) -> Directory -> [String] -> a
directory f g dic path = g dic path (map (either f (directory f g dic . (:path))) (dic M.! path))

size :: Directory -> [String] -> Integer
size = directory id (\_ _ -> sum)
