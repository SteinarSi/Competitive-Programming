import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            read
        >>> enumFromTo 1
        >>> simulate
        >>> print
    )

simulate :: [Int] -> Int
simulate []  = error "bruh"
simulate [x] = x
simulate xs = simulate (simul [] xs)
    where
        simul :: [Int] -> [Int] -> [Int]
        simul ys []       = reverse ys
        simul ys [x]      = tail (reverse (x:ys))
        simul ys (x:y:xs) = simul (x:ys) xs
