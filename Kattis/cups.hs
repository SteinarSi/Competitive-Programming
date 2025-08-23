import           Data.Char (isDigit)
import           Data.List (sort)

main :: IO ()
main = do
    cups <- fmap (map (parse . words) . tail . lines) getContents
    let sorted = map snd $ sort cups
    mapM_ putStrLn sorted

parse :: [String] -> (Int, String)
parse [a, b] | all isDigit a = (read a `div` 2, b)
             | otherwise = (read b, a)
