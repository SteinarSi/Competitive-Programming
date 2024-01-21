main :: IO ()
main = do
    [x1,y1,x2,y2] <- fmap (map (read::String->Double) . words) getLine
    print (abs (x2-x1) * abs (y2-y1))
