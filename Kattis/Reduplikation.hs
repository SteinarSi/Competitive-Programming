main :: IO ()
main = do
    (word:digit:_) <- lines <$> getContents
    putStrLn (concat (replicate (read digit) word))
