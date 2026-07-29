import           Control.Arrow ((>>>))

main :: IO ()
main = interact (read >>> flip take tribonacci >>> map show >>> unlines)

tribonacci :: [Integer]
tribonacci = 1 : 2 : 3 : zipWith3 (\a b c -> a+b+c) tribonacci (drop 1 tribonacci) (drop 2 tribonacci)
