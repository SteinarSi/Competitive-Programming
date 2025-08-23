main :: IO ()
main = interact (show . sum . map fight . tail . words)

fight :: String -> Int
fight []           = 1
fight [x]          = 1
fight ('C':'D':xs) = 0
fight (x:xs)       = fight xs
