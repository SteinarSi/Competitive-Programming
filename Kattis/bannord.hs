main :: IO ()
main = do
    fs <- getLine
    getLine >>= putStrLn . unwords . map (censor fs) . words

censor :: String -> String -> String
censor fs xs | any (`elem` xs) fs = replicate (length xs) '*'
             | otherwise          = xs
