import Control.Monad (forM_)

main :: IO ()
main = do
    inn <- getLine
    if inn == "end"
        then putStrLn "end"
        else do
            forM_ [0..6] $ \i -> putStrLn (join (map (format i) inn))
            putStr "\n\n"
            main

join :: [String] -> String
join [] = ""
join [s] = s
join (x:xs) = x ++ "  " ++ join xs

format :: Int -> Char -> String
format 2 ':' = "o"
format 4 ':' = "o"
format _ ':' = " "
format 0 '0' = "+---+"
format 1 '0' = "|   |"
format 2 '0' = "|   |"
format 3 '0' = "+   +"
format 4 '0' = "|   |"
format 5 '0' = "|   |"
format 6 '0' = "+---+"
format 0 '1' = "    +"
format 3 '1' = "    +"
format 6 '1' = "    +"
format _ '1' = "    |"
format 0 '2' = "+---+"
format 1 '2' = "    |"
format 2 '2' = "    |"
format 3 '2' = "+---+"
format 4 '2' = "|    "
format 5 '2' = "|    "
format 6 '2' = "+---+"
format 0 '3' = "+---+"
format 3 '3' = "+---+"
format 6 '3' = "+---+"
format _ '3' = "    |"
format 0 '4' = "+   +"
format 1 '4' = "|   |"
format 2 '4' = "|   |"
format 3 '4' = "+---+"
format 4 '4' = "    |"
format 5 '4' = "    |"
format 6 '4' = "    +"
format 1 '5' = "|    "
format 2 '5' = "|    "
format 4 '5' = "    |"
format 5 '5' = "    |"
format _ '5' = "+---+"
format 1 '6' = "|    "
format 2 '6' = "|    "
format 4 '6' = "|   |"
format 5 '6' = "|   |"
format _ '6' = "+---+"
format 0 '7' = "+---+"
format 3 '7' = "    +"
format 6 '7' = "    +"
format _ '7' = "    |"
format 0 '8' = "+---+"
format 3 '8' = "+---+"
format 6 '8' = "+---+"
format _ '8' = "|   |"
format 1 '9' = "|   |"
format 2 '9' = "|   |"
format 4 '9' = "    |"
format 5 '9' = "    |"
format _ '9' = "+---+"
