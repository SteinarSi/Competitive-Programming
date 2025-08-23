import Data.Set (Set, empty, member, insert)

main :: IO ()
main = getContents >>= (mapM_ putStrLn . process empty [] 12) . map words . tail . lines

process :: Set String -> [String] -> Int -> [[String]] -> [String]
process _ ret 0 _ = reverse ret
process taken ret n (t@[uni, team] : xs) | member uni taken = process taken ret n xs
                                         | otherwise = process (insert uni taken) (unwords t : ret) (pred n) xs
