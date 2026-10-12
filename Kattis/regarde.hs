import           Control.Arrow ((>>>))

main :: IO ()
main = interact (
            lines
        >>> drop 1
        >>> map (\n -> 's' : replicate (read n + 1) 'h')
        >>> unlines
    )
