import           Data.List (isPrefixOf)

main :: IO ()
main = interact (solve 0)

solve :: Int -> String -> String
solve _ "" = "no trees here"
solve i xs | "tree" `isPrefixOf` xs = show i
           | otherwise              = solve (i+1) (drop 1 xs)
