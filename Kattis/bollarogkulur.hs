import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            lines
        >>> play "1"
        >>> putStrLn
    )

play :: String -> [String] -> String
play ret [] = ret
play ret (x:y:xs)
    | x == ret  = play y xs
    | y == ret  = play x xs
    | otherwise = play ret xs
