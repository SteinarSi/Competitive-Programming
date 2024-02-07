import           Data.Char (isDigit)

main :: IO ()
main = getContents >>= print . maximum . findNumbers . last . lines

findNumbers :: String -> [Int]
findNumbers "" = []
findNumbers (x:xs) | isDigit x = read (x : takeWhile isDigit xs) : findNumbers (dropWhile isDigit xs)
                   | otherwise = findNumbers xs
