import Data.List (group)

main :: IO ()
main = getLine >>= print . maximum . map length . group