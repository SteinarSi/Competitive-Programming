import Data.List (foldr1)

main :: IO ()
main = getContents >>= print . foldr1 subtract . map read . words
