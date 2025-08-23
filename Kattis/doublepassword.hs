main :: IO ()
main = interact (show . (2^) . length . filter id . (\(a:b:_) -> zipWith (/=) a b) . words) >> putChar '\n'