import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (
            read
        >>> xorsistinn
        >>> putStrLn
    )

xorsistinn :: Int -> String
xorsistinn n = case n `mod` 4 of
        0 -> show n
        1 -> "1"
        2 -> "Gunnar"
        3 -> "Enginn"
