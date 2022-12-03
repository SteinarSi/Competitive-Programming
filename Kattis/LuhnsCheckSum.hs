import Data.Char

main = do
    input <- getLine
    checkSum (read input::Int)

checkSum :: Int -> IO ()
checkSum n
    | n == 0 = return ()
    | otherwise = do
         input <- getLine
         if luhnPlus input then do
             putStrLn "PASS"
             checkSum (n-1)
         else do
             putStrLn "FAIL"
             checkSum (n-1)

luhnDouble :: Int -> Int
luhnDouble n
    | n*2>9 = n*2-9
    | otherwise = n*2

luhnPlus :: String -> Bool
luhnPlus n = mod (luhnSummer ([digitToInt x | x<-n]) False) 10 == 0

luhnSummer :: [Int] -> Bool -> Int
luhnSummer [] _ = 0
luhnSummer xs b
    | b = luhnDouble (last xs) + (luhnSummer (init xs) (not b))
    | otherwise = (last xs) + (luhnSummer (init xs) (not b))