import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (read >>> days >>> print)

days :: Int -> Int
days 2 = 28
days n | n `elem` [1,3,5,7,8,10,12] = 31
       | otherwise = 30
