main :: IO ()
main = do
    xs <- getLine
    let x = length (filter ('0'==) xs)
    putStrLn (show x <> " " <> show (length xs - x))
