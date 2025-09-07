main :: IO ()
main = do
    [_,_,y,_] <- fmap words getContents
    print ((read y+20)*2)
