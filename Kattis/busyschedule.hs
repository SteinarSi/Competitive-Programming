import Data.List (sortOn)
import Control.Monad (unless, replicateM) 

main :: IO ()
main = do
    n <- fmap read getLine
    unless (n == 0) $ replicateM n getLine >>= putStrLn . unlines . sortOn (key . words) >> main

key :: [String] -> (String, Int, Int)
key [h:':':m, z] = (z, read [h] `mod` 12, read m)
key [h:h2:':':m, z] = (z, read [h,h2] `mod` 12, read m)
