main :: IO ()
main = getLine >>= \n-> mapM (const getLine) [1..read n] >>= \c -> print $ length $ filter id $ zipWith (==) c $ tail c