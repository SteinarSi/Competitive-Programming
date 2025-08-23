import           Data.List (isInfixOf, transpose)

main :: IO ()
main = do
    rows <- fmap (tail . lines) getContents
    let cols = transpose rows

    print $ if all correct (rows ++ cols)
        then 1
        else 0

correct :: String -> Bool
correct xs = count 'W' xs == count 'B' xs && not ("WWW" `isInfixOf` xs) && not ("BBB" `isInfixOf` xs)

count :: Eq a => a -> [a] -> Int
count a = length . filter (a==)
