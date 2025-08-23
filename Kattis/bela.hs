main :: IO ()
main = do
    _:[s]:xs <- fmap words getContents
    print $ sum $ map (convert s) xs

convert :: Char -> String -> Integer
convert d (v:s:_) = con (d == s) v
    where
        con :: Bool -> Char -> Integer
        con _ 'A' = 11
        con _ 'K' = 4
        con _ 'Q' = 3
        con True 'J' = 20
        con False 'J' = 2
        con _ 'T' = 10
        con True '9' = 14
        con _ _ = 0