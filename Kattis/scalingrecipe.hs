main :: IO ()
main = interact (unlines . (\(_:x:y:is) -> map (show . round . (/x) . (y*)) is) . map read . words)
