import Control.Arrow ((>>>))

main :: IO ()
main = getContents >>= (
            lines
        >>> last
        >>> flip (consume 1) champernowne
        >>> print
    )

consume :: Int -> String -> [String] -> Int
consume ret "" _ = ret
consume ret (x:xs) ("":yss) = consume (ret+1) (x:xs) yss
consume ret (x:xs) ((y:ys):yss) | x == y    = consume ret xs     (ys:yss)
                                | otherwise = consume ret (x:xs) (ys:yss)

champernowne :: [String]
champernowne = map show [1..]
