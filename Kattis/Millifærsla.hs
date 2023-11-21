main :: IO ()
main = getContents >>= putStrLn . snd . minimum . flip zip ["Monnei", "Fjee", "Dolladollabilljoll"] . map (read :: String -> Int) . words
