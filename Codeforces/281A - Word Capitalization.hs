import           Data.Char (toUpper)

main :: IO ()
main = do
    x:xs <- getLine
    putStrLn (toUpper x : xs)
