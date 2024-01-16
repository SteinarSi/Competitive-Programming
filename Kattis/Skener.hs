main :: IO ()
main = do
    [_,_,rows,cols] <- fmap (map read . words) getLine
    xs <- fmap lines getContents
    let enlarged = widen cols . heighten rows $ xs
    mapM_ putStrLn enlarged

widen :: Int -> [String] -> [String]
widen k = map (concatMap (replicate k))

heighten :: Int -> [String] -> [String]
heighten k = concatMap (replicate k)
