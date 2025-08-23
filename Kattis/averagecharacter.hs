import Data.Char (ord, chr)

main :: IO ()
main = do
    word <- fmap (map ord) getLine
    putStr (chr (sum word `div` length word) : "\n")
