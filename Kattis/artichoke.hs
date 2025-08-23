main :: IO ()
main = do
    (p:a:b:c:d:n:_) <- getLine >>= return . map read . words
    let m = maximum $ drops (-2147483648) [ p * (sin(a*k + b) + cos(c*k + d) + 2) | k<-[1..n] ]
    if m > 0 then print m
    else putChar '0'

drops :: (Ord n, Num n) => n -> [n] -> [n]
drops _ [] = []
drops m (x:xs) = (m-x) : drops (max m x) xs