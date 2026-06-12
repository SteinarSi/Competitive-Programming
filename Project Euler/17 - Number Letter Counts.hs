import           Data.Char     (isAlpha)
import           Data.Function ((&))

main :: IO ()
main = concatMap (filter isAlpha) numbers
    & length
    & print

ones :: [String]
ones = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens :: [String]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

doubleDigits :: [String]
doubleDigits = ones <> teens <> concatMap (\t -> t : map ((t <> "-") <> ) ones) tens

hundreds :: [String]
hundreds = concatMap (\h -> (h <> " hundred") : map ((h <> " hundred and ") <>) doubleDigits) ones

numbers :: [String]
numbers = doubleDigits <> hundreds <> ["one thousand"]
