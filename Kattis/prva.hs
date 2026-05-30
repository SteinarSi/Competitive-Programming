import           Control.Arrow ((>>>))
import           Data.List     (transpose)

main :: IO ()
main = do
    rows <- fmap (lines >>> drop 1) getContents
    putStrLn (minimum (concatMap (splitOn '#' >>> filter (length >>> (>=2))) (rows ++ transpose rows)))

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = (x : takeWhile (/=p) xs) : splitOn p (dropWhile (/=p) xs)
