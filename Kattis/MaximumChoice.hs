import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,b] <- getLine <&> (words >>> map read)
    print (solve (b+1) n)

solve :: Double -> Double -> Int
solve d n | n <  d    = 1
          | otherwise = 1 + solve d (n / d)
