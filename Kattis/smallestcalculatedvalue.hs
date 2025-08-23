import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b,c] <- getLine <&> (words >>> map read)

    calculations a b c
        & filter (>=0)
        & minimum
        & print

calculations :: Int -> Int -> Int -> [Int]
calculations a b c = operations a b >>= (`operations` c)

operations :: Int -> Int -> [Int]
operations a b | mod a b == 0 = a `div` b : r
               | otherwise = r
    where r = [a+b,a*b,a-b]
