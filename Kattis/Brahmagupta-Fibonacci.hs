import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Text.Printf   (printf)

main :: IO ()
main = do
    [a,b,c,d] <- getContents <&> (words >>> map read)

    let ac = a*c :: Integer
        ad = a*d
        bc = b*c
        bd = b*d
        pq = (a^2 + b^2) * (c^2 + d^2)

        format x | x < 0     = printf "(%d)" x
                 | otherwise = show x

    printf "%s^2 + %s^2 = %d\n" (format (ac-bd)) (format (ad+bc)) pq
    printf "%s^2 + %s^2 = %d\n" (format (ac+bd)) (format (ad-bc)) pq
