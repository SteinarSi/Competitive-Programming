import           Data.List (sort)

main :: IO ()
main = do
    xs <- fmap (sort . map read . tail . words) getContents
    let (lost, won) = splitAt (length xs `div` 2 + 1) xs
    print $ sum (won ++ map (`div` 2) lost)
