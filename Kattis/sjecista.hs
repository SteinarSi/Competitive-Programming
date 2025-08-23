main = do
    input <- getLine
    let x = read input::Integer
    if x == 3 then putStrLn ("0")
    else print( (foldl (*) 1 [x, (x-1)..(4+1)]) `div`(fac (x-4))   ) 

fac :: Integer -> Integer
fac x = foldl (*) 1 [x, x-1..1]