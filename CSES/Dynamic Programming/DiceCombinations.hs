import Data.Array (Array, (!), listArray)

main :: IO ()
main = getLine >>= print . (answers !) . read

diceUp :: Array Int Integer -> Int -> Integer
diceUp arr n | n <= 0 = 0
             | otherwise = arr ! n

answers :: Array Int Integer
answers = listArray (1, 1000000) ([1, 2, 4, 8, 16, 32] ++ [ mod (sum [ diceUp answers (i-j) | j <- [1..6] ]) 1000000007 | i<-[7..1000000]])