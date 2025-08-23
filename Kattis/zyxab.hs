import           Data.List (maximumBy, nub)

main :: IO ()
main = do
    names <- fmap (filter (\x -> x == nub x && length x >= 5) . tail . lines) getContents
    putStrLn $
        if null names
            then "Neibb"
            else maximumBy cmp names

cmp :: String -> String -> Ordering
cmp xs ys
    | lx > ly = LT
    | lx < ly = GT
    | otherwise = compare xs ys
    where
        lx = length xs
        ly = length ys
