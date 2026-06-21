import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (
            read
        >>> divisors
        >>> map show
        >>> unlines
        >>> putStr
    )

divisors :: Int -> [Int]
divisors x = divs [] [1..]
  where
    divs qs (d:ds)
        | d*d > x   = qs
        | r /= 0    = divs qs ds
        | d == q    = q : qs
        | otherwise = d : divs (q:qs) ds
      where
        (q,r) = quotRem x d
