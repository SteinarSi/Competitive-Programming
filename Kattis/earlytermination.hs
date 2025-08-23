import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Text.Printf   (printf)

main :: IO ()
main = do
    [a,b,k] <- getContents <&> (words >>> map read)

    uncurry (printf "%d %d\n") (euclid (floor (sqrt (fromIntegral k))) (max a b) (min a b))

euclid :: Int -> Int -> Int -> (Int,Int)
euclid sk a 0  = (a,0)
euclid sk a b | a <= sk && b <= sk = (a,b)
              | otherwise = euclid sk b (a `mod` b)
