{-# LANGUAGE TypeApplications #-}

import Data.List (sort, sortBy, elemIndex)
import Data.Function (on)

main :: IO ()
main = do
    abc <- fmap (zip ['A'..] . sort . map (read @Int) . words) getLine
    order <- getLine
    let sorted = sortBy (compare `on` ((`elemIndex` order) . fst)) abc
    putStrLn . unwords $ map (show . snd) sorted
