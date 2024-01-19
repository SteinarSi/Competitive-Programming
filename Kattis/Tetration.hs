main :: IO ()
main = getLine >>= print . tetration . read

tetration :: Double -> Double
tetration y = y ** (1 / y)
