{-# LANGUAGE TypeApplications #-}

import           Data.List (foldl', maximumBy, sort)
import           Data.Map  (Map, assocs, elems, empty, insertWith)

main :: IO ()
main = do
    frosh <- fmap (elems . foldl' (\m c -> insertWith (+) c 1 m) empty . map (sort . map (read @Int) . words) . tail . lines) getContents
    let best  = maximum frosh
        count = length $ filter (==best) frosh
    print (best * count)
