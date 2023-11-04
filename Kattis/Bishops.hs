main :: IO ()
main = interact (unlines . map (show . bishop . read) . lines) >> putChar '\n'

bishop :: Int -> Int
bishop 0 = 0
bishop 1 = 1
bishop n = 2*n - 2
