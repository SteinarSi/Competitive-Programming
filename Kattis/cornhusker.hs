main :: IO ()
main = do
    avg <- fmap ((`div` 5) . kernelSizes . map read . words) getLine
    [ears, kwf] <- fmap (map read . words) getLine
    print ((ears * avg) `div` kwf)

kernelSizes :: [Int] -> Int
kernelSizes []       = 0
kernelSizes (x:y:xs) = x*y + kernelSizes xs
kernelSizes _        = error "Bruh"
