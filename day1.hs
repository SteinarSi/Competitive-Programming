import Data.List.Split (splitWhen)
import Data.List (sort)

main :: IO ()
main = readFile "day1-input.txt" >>= print .  foldr (\x xs -> max (sum $ map read x) xs) 0 . splitWhen null . lines

main2 :: IO ()
main2 = readFile "day1-input.txt" >>= print . sum . take 3 . reverse . sort . map (sum . map read) . splitWhen null . lines


