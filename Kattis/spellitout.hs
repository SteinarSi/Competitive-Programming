{-# LANGUAGE MultiWayIf #-}

import           Data.Functor ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read
    let (q,r) = quotRem n 10
    putStrLn $ if
        | n < 20    -> (singles <> teens) !! n
        | r == 0    -> tens !! q
        | otherwise -> tens !! q <> "-" <> singles !! r

singles :: [String]
singles = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens :: [String]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
