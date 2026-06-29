import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,s] <- getLine <&> (words >>> map read)
    let d = facDigits n
        z = facZeros n
    if z > s
        then putStrLn (show (d-z+s) <> " " <> show z)
        else print d

facZeros :: Int -> Int
facZeros x = map (5^) [1..]
    & takeWhile (<=x)
    & map (div x)
    & sum

facDigits :: Int -> Int
facDigits n
    | n <= 1 = 1
    | otherwise = ceiling (sum (map (fromIntegral >>> logBase 10) [1..n]))
