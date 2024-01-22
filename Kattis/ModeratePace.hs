import           Data.List (sort)

main :: IO ()
main = do
    getLine
    [xs, ys, zs] <- fmap (map (map read . words) . lines) getContents
    putStrLn . unwords . map show $ zipWith3 median xs ys zs

median :: Int -> Int -> Int -> Int
median a b c = sort [a, b, c] !! 1
