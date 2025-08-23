main :: IO ()
main = do
    students <- fmap (map (parse . words) . tail . lines) getContents
    mapM_ (putStrLn . eligible) students

parse :: [String] -> (String, Int, Int, Int)
parse [name, start, birth, courses] = (name, read (take 4 start), read (take 4 birth), read courses)

eligible :: (String, Int, Int, Int) -> String
eligible (name, start, birth, courses) | start >= 2010 = name ++ " eligible"
                                       | birth >= 1991 = name ++ " eligible"
                                       | courses > 40 = name ++ " ineligible"
                                       | otherwise = name ++ " coach petitions"

