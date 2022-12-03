main :: IO ()
main = getLine >>= print . move 1

move :: Int -> String -> Int
move r [] = r
move r (c:cs) | elem (c, r) [('C', 3), ('A', 2)] = move 1 cs
              | elem (c, r) [('A', 1), ('B', 3)] = move 2 cs
              | elem (c, r) [('C', 1), ('B', 2)] = move 3 cs
              | otherwise = move r cs