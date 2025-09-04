import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (read >>> solve >>> print)

solve :: Int -> Int
solve x = case digits x of
    [y] -> y
    ds  -> solve (sum ds)

digits :: Int -> [Int]
digits 0 = []
digits x = let (q,r) = quotRem x 10
           in  r : digits q
