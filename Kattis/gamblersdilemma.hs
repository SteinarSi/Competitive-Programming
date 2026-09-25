import           Control.Arrow ((>>>))
import           Data.Char     (digitToInt)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [(r1,s1),(r2,s2)] <- getLine <&> (words >>> map parse)

    let play = (r1 == r2 && r1 > 3) || (r1+1 == r2 && r1 >= 9) || (r2+1 == r1 && r2 >= 9) || (s1 == s2 && (r1 >= 12 || r2 >= 12)) || (r1 >= 11 && r2 >= 11)

    putStrLn $ if play
        then "yes"
        else "no"

parse :: String -> (Int,Char)
parse [r,s] = (,s) $ case r of
    'T' -> 10
    'J' -> 11
    'Q' -> 12
    'K' -> 13
    'A' -> 14
    _   -> digitToInt r
