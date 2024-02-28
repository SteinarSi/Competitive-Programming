import           Data.Set (Set, delete, fromList, member)

main :: IO ()
main = do
    [pass, text] <- fmap words getLine
    putStrLn $ solve pass text (fromList pass)

solve :: String -> String -> Set Char -> String
solve [] _ _ = "PASS"
solve _ [] _ = "FAIL"
solve (x:xs) (y:ys) bans | x == y && x `notElem` xs = solve xs ys (delete x bans)
                         | x == y = solve xs ys bans
                         | member y bans = "FAIL"
                         | otherwise = solve (x:xs) ys bans
