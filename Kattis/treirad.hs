main :: IO ()
main = getLine >>= print . length . flip takeWhile triplets . flip (<) . read

triplets :: [Int]
triplets = map (\x -> x * (x+1) * (x+2)) [1..]
