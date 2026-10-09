main :: IO ()
main = do
    [xs,ys,ss,ri,rf,xa,ya,sa] <- map read . words <$> getContents

    let ds = ri-rf
        da = sqrt ((xa-xs)^2 + (ya-ys)^2) - rf
        ts = ds / ss
        ta  = da / sa
        to = abs ((ds-da) / (ss-sa))

    print $ case (compare da ds, ta <= ts) of
        (LT,True)  -> 0
        (LT,False) -> ta - to
        (GT,True)  -> to
        (GT,False) -> ta
        (EQ,True)  -> 0
        (EQ,False) -> ta
