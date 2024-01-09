import           Data.Bool (bool)

main :: IO ()
main = do
    _:w:h:xs <- fmap (map ((^2) . read) . words) getContents
    mapM_ (\x -> putStrLn (bool "NE" "DA" (x <= w + h))) xs
