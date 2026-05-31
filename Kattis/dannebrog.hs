main :: IO ()
main = do
    w <- read <$> getLine
    print (round (20*(6*w / 28)^^2))
