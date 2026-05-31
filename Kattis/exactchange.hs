import           Control.Arrow ((>>>))

main :: IO ()
main = interact (
            read
        >>> change [150,30,15,5,1]
        >>> reverse
        >>> map show
        >>> unwords
    )

change :: [Int] -> Int -> [Int]
change [] _ = []
change (x:xs) m = q : change xs r
  where
    (q,r) = quotRem m x
