main :: IO ()
main = do
    n <- fmap read getContents

    let (q1, r1) = quotRem n 400
        (q2, r2) = quotRem n 100
        (q3, r3) = quotRem n 4

    putStrLn $ if r1 == 0 || r3 == 0 && r2 /= 0
        then show (q1 + q3 - q2 - 490)
        else "Neibb"
