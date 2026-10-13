main :: IO ()
main = do
    [d,a,u,da,du] <- map read . words <$> getContents

    let pa = max 0 (a - d*da)
        pu = max 0 (u - d*du)

    print (100 * pa / (pa + pu))
