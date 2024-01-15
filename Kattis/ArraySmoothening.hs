import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.List             (sort, sortBy)
import           Data.Ord              (Down (Down), comparing)

main :: IO ()
main = do
    n:k:_ <- fmap (map read . words) getLine
    xs <- fmap (sortBy (comparing Down) . counts . sort . BC.words) B.getContents
    print $ smooth (head xs) 0 k xs

counts :: Eq a => [a] -> [Int]
counts [] = []
counts (x:xs) = c: counts rest
    where c = 1 + length (takeWhile (x==) xs)
          rest = dropWhile (x==) xs

smooth :: Int -> Int -> Int -> [Int] -> Int
smooth level _ 0 _ = level
smooth level stacks k [] = max 0 (level - k `div` stacks)
smooth level stacks k (x:xs) | level == x = smooth level (stacks+1) k xs
                             | k >= cost  = smooth x (stacks+1) (k-cost) xs
                             | otherwise  = smooth level stacks k []
    where cost = (level - x) * stacks
