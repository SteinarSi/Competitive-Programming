import           Data.Bifunctor (bimap, first, second)

main :: IO ()
main = do
    xs <- getLine
    let (x, y) = convert xs
    putStrLn $ unwords $ map show [length xs, x, y]

convert :: String -> (Int, Int)
convert "" = (0, 0)
convert (x:xs) = case x of
    '0' -> convert xs
    '1' -> first move (convert xs)
    '2' -> second move (convert xs)
    '3' -> bimap move move (convert xs)
    where move = (2^length xs +)
