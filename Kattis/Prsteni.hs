main :: IO ()
main = do
    a <- getLine
    let antall = read a::Int
    inputs <- getLine
    let tall = [read x::Int | x<- words inputs]
    printFractions (head tall) (tail tall)

printFractions :: Int -> [Int] -> IO()
printFractions _ [] = return ()
printFractions teller (t:ts) = do
    putStrLn ((show x) ++ "/" ++ (show y))
    printFractions teller ts
        where (x, y) = fractionize teller t 2

fractionize :: Integral a => a -> a -> a -> (a, a)
fractionize t n d
    | t < d = (t, n)
    | mod t d == 0 && mod n d == 0 = fractionize (div t d) (div n d) d
    | otherwise = fractionize t n (d+1)