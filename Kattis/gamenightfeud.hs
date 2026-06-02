import           Data.Functor ((<&>))
import           Data.List    (transpose)

main :: IO ()
main = do
    p1:p2:xss <- getContents <&> words
    let board = xss <> transpose xss <> [map (\i -> xss!!i!!i) [0..2], map (\i -> xss!!i!!(2-i)) [0..2]]
        xs = count 'X' (concat xss)
        os = count 'O' (concat xss)
        wx = count "XXX" board
        wo = count "OOO" board
    putStrLn $ case (wx,wo) of
        _ | xs < os || xs > os+1   -> "Invalid Game"
        (1,0)                      -> p1
        (0,1)                      -> p2
        (0,0) | xs == 5 && os == 4 -> "Draw"
              | otherwise          -> "In-Progress"
        _                          ->  "Invalid Game"

count :: Eq a => a -> [a] -> Int
count x xs = length (filter (x==) xs)
