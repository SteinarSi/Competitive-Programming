main :: IO ()
main = do
    x <- read <$> getLine
    print ((60*60 + 59 - x `mod` (60*60)) `div` 60)
