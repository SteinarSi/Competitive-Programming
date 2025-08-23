main :: IO ()
main = do
    n <- fmap read getLine
    print (days !! n)
    print (total !! n)

days :: [Int]
days = scanl (+) 0 [1..]

total :: [Int]
total = scanl1 (+) days
