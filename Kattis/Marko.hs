import Data.Char

main :: IO ()
main = do
    input <- getLine
    wordslist <- takeWordList (read input::Int)
    keys <- getLine
    let filtered = filterWords wordslist (zip keys [0..])
    print (length filtered)
    

alphabet :: [[Char]]
alphabet = ["abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz"]

chars :: Char -> [Char]
chars n = alphabet !! ((digitToInt n)-2)

takeWordList :: Int -> IO [String]
takeWordList 0 = return []
takeWordList n = do
    x <- getLine
    xs <- takeWordList (n-1)
    return (x:xs)

filterWords :: [String] -> [(Char, Int)] -> [String]
filterWords wrds [] = wrds
filterWords wrds ((c, i):xs) = filterWords [w | w<-wrds, elem (w!!i) (chars c)] xs