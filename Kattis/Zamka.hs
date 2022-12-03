import Data.Char(digitToInt)
import Data.List (find)
import Data.Maybe (fromJust)

main :: IO()
main = do
    l <- read <$> getLine
    d <- read <$> getLine
    x <- read <$> getLine :: IO Int
    print (fromJust (find ((x==) . sumDigits) [l..]))
    print (fromJust (find ((x==) . sumDigits) [d, d-1..]))

sumDigits :: Int -> Int
sumDigits = sum . map digitToInt . show
