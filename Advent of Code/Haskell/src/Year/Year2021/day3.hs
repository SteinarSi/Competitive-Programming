import Data.List ( findIndices )
import Data.Char (digitToInt)


main :: IO ()
main = do
    s <- readFile "day3-input.txt"
    let 
        xs = lines s
        b = count (replicate 12 (0, 0)) xs
        inverse = map (\n -> if n==0 then 1 else 0) b

        gamma = binToDec b
        epsilon = binToDec inverse

        oxygen = filterNumbers xs 0 True
        co2 = filterNumbers xs 0 False

    putStrLn ("Gamma*epsilon: " ++ show (gamma * epsilon))
    putStrLn ("oxygen*c02: " ++ show (binToDec (map digitToInt oxygen) * binToDec (map digitToInt co2)))

count :: [(Int, Int)] -> [String] -> [Int]
count r [] = map (\(one, zero) -> if one > zero then 1 else 0) r
count r (s:xs) = count (zipWith (\(one, zero) d -> if d=='1' then (one+1, zero) else (one, zero+1)) r s) xs

countByIndex :: (Int, Int) -> [String] -> Bool -> Int -> Char
countByIndex (one, zero) [] b i | b = if one >= zero then '1' else '0'
                                | otherwise = if zero <= one then '0' else '1'
countByIndex (one, zero) (x:xs) b i | x!!i == '1' = countByIndex (one+1, zero) xs b i
                                    | otherwise = countByIndex (one, zero+1) xs b i


filterNumbers :: [String] -> Int -> Bool -> String
filterNumbers [s] _ _ = s
filterNumbers xs i b = filterNumbers (filter (\x -> x!!i == rule) xs) (i+1) b
    where rule = countByIndex (0, 0) xs b i

testList :: [String]
testList = ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]

binToDec :: [Int] -> Int
binToDec l = sum $ map (2^) $ findIndices (==1) $ reverse l