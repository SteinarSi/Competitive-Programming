main :: IO ()
main = do
    [_,xs,ys] <- lines <$> getContents

    let score = sum (zipWith (\x y -> if elem (x,y) wins then 1 else if elem (y,x) wins then -1 else 0) xs ys)

    putStrLn $ if score > 0
        then "victory"
        else "defeat"

wins :: [(Char,Char)]
wins = [('R','S'),('S','P'),('P','R')]
