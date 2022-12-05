
-- TODO
main :: IO ()
main = fmap (map read . words) getLine >>= \[b, k, g] -> print (b `div` (k `div` g))
