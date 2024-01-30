import           Data.List (sort)

main :: IO ()
main = getContents >>= print . maximum . zipWith (+) [2..] . reverse . sort . map read . tail . words
