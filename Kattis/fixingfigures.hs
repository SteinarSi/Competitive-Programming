import           Control.Arrow ((>>>))

main :: IO ()
main = interact (
            lines
        >>> drop 1
        >>> map (read >>> solve)
        >>> unlines
    )

solve :: Int -> String
solve x | x < 0 = "negative " <> solve (-x)
        | otherwise = case quotRem x 1000000 of
            (0,t) -> case quotRem t 1000 of
                (0,h) -> case quotRem h 100 of
                    (0,d) -> case d of
                        0  -> "zero"
                        1  -> "one"
                        2  -> "two"
                        3  -> "three"
                        4  -> "four"
                        5  -> "five"
                        6  -> "six"
                        7  -> "seven"
                        8  -> "eight"
                        9  -> "nine"
                        10 -> "ten"
                        11 -> "eleven"
                        12 -> "twelve"
                        13 -> "thirteen"
                        14 -> "fourteen"
                        15 -> "fifteen"
                        16 -> "sixteen"
                        17 -> "seventeen"
                        18 -> "eighteen"
                        19 -> "nineteen"
                        20 -> "twenty"
                        30 -> "thirty"
                        40 -> "forty"
                        50 -> "fifty"
                        60 -> "sixty"
                        70 -> "seventy"
                        80 -> "eighty"
                        90 -> "ninety"
                        _  -> solve (10 * (d `div` 10)) <> "-" <> solve (d `mod` 10)
                    (c,0) -> solve c <> " hundred"
                    (c,d) -> solve c <> " hundred " <> solve d
                (t,0) -> solve t <> " thousand"
                (t,h) -> solve t <> " thousand " <> solve h
            (m,0) -> solve m <> " million"
            (m,t) -> solve m <> " million " <> solve t
