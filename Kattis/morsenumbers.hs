import           Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            words
        >>> drop 1
        >>> message
        >>> print
    )

message :: [String] -> Int
message xss = sum (map number xss) + 7 * (length xss - 1)

number :: String -> Int
number xs = sum (map digit xs) + 3 * (length xs - 1)

digit :: Char -> Int
digit x = sum ds + length ds - 1
  where
    ds = case x of
        '0' -> [dash, dash, dash, dash, dash]
        '1' -> [dit , dash, dash, dash, dash]
        '2' -> [dit , dit , dash, dash, dash]
        '3' -> [dit , dit , dit , dash, dash]
        '4' -> [dit , dit , dit , dit , dash]
        '5' -> [dit , dit , dit , dit , dit ]
        '6' -> [dash, dit , dit , dit , dit ]
        '7' -> [dash, dash, dit , dit , dit ]
        '8' -> [dash, dash, dash, dit , dit ]
        '9' -> [dash, dash, dash, dash, dit ]

    dit = 1
    dash = 3
