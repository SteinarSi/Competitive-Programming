main :: IO ()
main = do
    (a:b:'/':c:d:_) <- getLine
    putStrLn $ case (read [a,b], read [c,d]) of 
        (f, s) | f <= 12 && s <= 12 -> "either"
               | f <= 12            -> "US"
               | otherwise          -> "EU"
