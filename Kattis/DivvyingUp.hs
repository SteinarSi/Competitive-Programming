import           Data.Bool (bool)

main :: IO ()
main = getContents >>= putStrLn . bool "no" "yes" . (==0) . (`mod` 3) . sum . map read . tail . words

-- Very glad each individual price can be split, so that the problem isn't NP-complete
