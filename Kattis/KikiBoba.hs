import Data.Ord (Ordering(..), compare)

main :: IO ()
main = do
    inn <- getLine
    putStrLn $ case compare (count 'b' inn) (count 'k' inn) of
        LT -> "kiki"
        GT -> "boba"
        EQ | count 'b' inn == 0 -> "none"
           | otherwise -> "boki"
    
count :: Eq a => a -> [a] -> Int
count a = length . filter (a==)
