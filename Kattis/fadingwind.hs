import Data.Bool (bool)

main :: IO ()
main = interact (show . fly . map read . words) >> putChar '\n'

fly :: [Int] -> Int
fly [h, k, v, s] | h <= 0    = 0
                 | otherwise = v'' + fly [h', k, v'', s']
    where v'  = v + s - max 1 (div (v+s) 10)
          h'  | v' >= k   = h+1
              | v' <= 0   = 0
              | otherwise = h-1
          v'' = bool 0 v' (h' > 0)
          s'  = max 0 (s-1)
