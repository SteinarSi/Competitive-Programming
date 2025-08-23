import           Data.Char (isUpper)
main = getLine >> getLine >>= putStrLn . filter isUpper . map head . words
