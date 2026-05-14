import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (read >>> solve >>> print)

solve :: Int -> Int
solve 1 = 1
solve n | odd n     = 2 * solve (n `div` 2) + 1
        | otherwise = 2 * solve (n `div` 2) - 1
