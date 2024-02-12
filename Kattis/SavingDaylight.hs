import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            lines
        >>> mapM_ (words >>> parse >>> solve >>> putStrLn)
    )

parse :: [String] -> (String, (Int,Int), (Int,Int))
parse [month, day, year, from, to] = (unwords [month, day, year], (h1,m1), (h2,m2))
    where [h1,m1, h2,m2] = concatMap (splitOn ':' >>> map read) [from, to]

solve :: (String,(Int,Int),(Int,Int)) -> String
solve (name, (h1,m1), (h2,m2)) = unwords [name, show hours, "hours", show minutes, "minutes"]
    where hours | m2 < m1   = h2-h1-1
                | otherwise = h2-h1
          minutes | m2 < m1   = 60-m1 + m2
                  | otherwise = m2 - m1

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p (x:xs) | p == x    = splitOn p xs
                 | otherwise = (x : takeWhile (/=p) xs) : splitOn p (dropWhile (/=p) xs)
