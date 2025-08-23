import           Data.Array (Array, array, (!))

main :: IO ()
main = getLine >>= print . plank . read

plank :: Integer -> Integer
plank n = arr ! n
    where arr = array (0, n) $ (0, 1) : map (\i -> (i, sum [ arr ! (i-d) | d <- [1..3], i - d >= 0 ])) [1..n]
