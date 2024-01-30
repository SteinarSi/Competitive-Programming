import           Control.Arrow   ((>>>))
import           Data.Function   ((&))
import           Data.List       (transpose)
import           Data.List.Split (chunksOf)


main :: IO ()
main = do
    rows <- fmap (tail . lines) getContents
    let cols = transpose rows
        words = concatMap (filter (length >>> (>=2)) . splitOn '#') (rows ++ cols)

    putStrLn $ minimum words

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = (x : takeWhile (/=p) xs) : splitOn p (dropWhile (/=p) xs)
