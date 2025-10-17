main :: IO ()
main = do
    l <- last <$> getLine
    putStrLn $ if l == '0'
        then "10"
        else [l]
