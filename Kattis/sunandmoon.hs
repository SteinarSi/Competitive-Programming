main :: IO ()
main = interact (show . solve . map read . words) >> putChar '\n'

solve :: [Int] -> Int
solve [0, _ , 0, _ ] = 0
solve [s, sm, m, mm] = 1 + solve [mod (s+1) sm, sm, mod (m+1) mm, mm]
