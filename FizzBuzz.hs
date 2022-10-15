main = do
    input <- getLine
    let split = words input
    fizzBuzz2 1 (read (split!!0)::Int) (read(split!!1)::Int) (read(split!!2)::Int)

fizzBuzz2 :: Int -> Int -> Int -> Int -> IO()
fizzBuzz2 i fizz buzz n = do
    if i == (n+1) then return()
    else do
    if i `mod` fizz == 0 && i `mod` buzz == 0 then putStrLn "FizzBuzz"
    else if i `mod` fizz == 0 then putStrLn "Fizz"
    else if i `mod` buzz == 0 then putStrLn "Buzz"
    else print i
    fizzBuzz2 (i+1) fizz buzz n