import           Data.List (sort)

main :: IO ()
main = do
    n:m:xs <- fmap (map read . words) getContents
    let (kids, rem) = splitAt m (replicate (2*m - n) 0 ++ sort xs)
    print . maximum $ zipWith (+) kids (reverse rem)
