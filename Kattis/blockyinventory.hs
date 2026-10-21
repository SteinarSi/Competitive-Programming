import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [d,s,b,n] <- getLine <&> (words >>> map read)

    let n' = b*9 + n

    [max 0 ((n' - 128) `div` 9) .. n' `div` 9]
        & map (\b' -> slots d + slots s + slots b' + slots (n' - 9*b'))
        & minimum
        & print

slots :: Int -> Int
slots x = (x+63) `div` 64
