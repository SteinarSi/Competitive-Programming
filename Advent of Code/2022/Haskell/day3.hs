import Data.Char (ord, isLower)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
    f <- fmap lines $ readFile "inputs/day3-input.txt"
    print . sum $ map (\s -> findPriority (splitAt (length s `div` 2) s)) f
    print . sum . map findBadge $ chunksOf 3 f

findPriority :: (String, String) -> Int
findPriority ((x:xs), ys) | elem x ys = priority x
                          | otherwise = findPriority (xs, ys)

findBadge :: [String] -> Int
findBadge [x:xs, ys, zs] | elem x ys && elem x zs = priority x
                         | otherwise = findBadge [xs, ys, zs]

priority :: Char -> Int
priority c | isLower c = ord c - 96
           | otherwise = ord c - 38
