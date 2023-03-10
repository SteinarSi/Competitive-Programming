import Data.List (sort)

main :: IO ()
main = do
    inn <- getLine
    if inn == "0" then return ()
    else do
        listeA <- mapM (\i -> getLine >>= return . read) [1..read inn]
        listeB <- mapM (\i -> getLine >>= return . read) [1..read inn]
        mapM_ print (synchronize (zip (sort listeA) (sort listeB)) listeA)
        main

synchronize :: [(Int, Int)] -> [Int] -> [Int]
synchronize _ [] = []
synchronize ab (x:xs) = [b | (a, b) <- ab, a==x] ++ synchronize ab xs