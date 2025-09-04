import           Control.Arrow ((>>>))

main :: IO ()
main = interact (words >>> drop 1 >>> init >>> deduplicate >>> unwords)

deduplicate :: [String] -> [String]
deduplicate []  = ["$\n"]
deduplicate [x] = [x,"$\n"]
deduplicate (x:y:xs) | x == y    =     deduplicate (y:xs)
                     | otherwise = x : deduplicate (y:xs)
