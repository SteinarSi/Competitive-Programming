main :: IO ()
main = mainsss 1

mainsss n = do
    input <- getLine
    let tall = read input::Int
    if tall == 0 then return ()
    else do
        ord <- takeInputs tall
        putStrLn ("SET " ++ (show n))
        printList (looper ord [])
        mainsss (n+1)

takeInputs :: Int -> IO[String]
takeInputs 0 = return []
takeInputs n = do
    x <- getLine
    xs <- takeInputs (n-1)
    return (x:xs)

looper :: [String] -> [String] -> [String]
looper [] temp = temp
looper [x] temp = [x] ++ temp 
looper (x:y:xs) temp = [x] ++ looper xs (y:temp)

printList :: [String] -> IO ()
printList [] = return ()
printList (x:xs) = do
    putStrLn x
    printList xs