main :: IO ()
main = do
    [a,b] <- map read . words <$> getContents

    putStrLn $ case (a>21, b>21) of
        (True,True)   -> "Jack"
        (True,False)  -> "Bertil"
        (False,True)  -> "Alice"
        (False,False) -> case compare a b of
            GT -> "Alice"
            LT -> "Bertil"
            EQ -> "Jack"
