main :: IO ()
main = do
    [p1,p2] <- fmap words getContents
    putStrLn $ if elem (p1,p2) wins
        then "Player 1"
        else if elem (p2,p1) wins
            then "Player 2"
            else "Draw"

wins :: [(String,String)]
wins = [("scissors","paper"), ("rock","scissors"), ("paper","rock")]
