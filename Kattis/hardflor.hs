import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            words
        >>> drop 1
        >>> parse (0,0)
        >>> shoelace
        >>> show
        >>> ("THE AREA IS " <>)
        >>> putStrLn
    )

parse :: (Int,Int) -> [String] -> [(Int,Int)]
parse (x,y) [] = [(x,y)]
parse (x,y) ((d:l):xs) = (x,y) : parse next xs
  where
    p = read l
    next = case d of
        'N' -> (x,y+p)
        'S' -> (x,y-p)
        'E' -> (x+p,y)
        'W' -> (x-p,y)

shoelace :: [(Int, Int)] -> Int
shoelace xs = abs (sum (zipWith (\(x1,y1) (x2,y2) -> (y1+y2) * (x1-x2)) xs (tail xs ++ [head xs]))) `div` 2
