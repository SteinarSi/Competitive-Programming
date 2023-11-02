import Data.List (sort)

main :: IO ()
main = getContents >>= (\(a:_:b:_) -> print (a*b)) . sort . map read . words
