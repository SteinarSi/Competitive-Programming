main :: IO ()
main = do
    [w,h,a,b] <- map read . words <$> getLine
    print (min (fit (a,b) (w,h)) (fit (a,b) (h,w)))

fit :: (Int,Int) -> (Int,Int) -> Int
fit (x,y) (w,h) = case (w <= x, h <= y) of
    (True , True ) -> 0
    (True , False) -> (h-y) * w
    (False, True ) -> (w-x) * h
    (False, False) -> w*h - x*y
