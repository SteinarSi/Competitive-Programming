import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b] <- getLine <&> (words >>> map read)
    print (solve a b)

solve :: Int -> Int -> Int
solve a b | a > b = 0
          | otherwise = 1 + solve (3*a) (2*b)
