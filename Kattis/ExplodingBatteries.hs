main :: IO ()
main = interact (unlines . map (show . solve . pred . read) . init . words)

solve :: Double -> Int
solve n = ceiling ((sqrt (1 + 8*n) - 1) / 2)
