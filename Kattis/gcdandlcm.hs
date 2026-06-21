import           Control.Arrow ((&&&), (>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Text.Printf   (printf)

main :: IO ()
main = do
    [x,y] <- getLine <&> (words >>> map read)
    divisors y
        & map (id &&& ((x*y)`div`))
        & filter (uncurry gcd >>> (==x))
        & map (uncurry (printf "%d %d"))
        & unlines
        & putStr

divisors :: Integer -> [Integer]
divisors x = divs [] [1..]
  where
    divs qs (d:ds)
        | d*d > x   = qs
        | r /= 0    = divs qs ds
        | d == q    = q : qs
        | otherwise = d : divs (q:qs) ds
      where
        (q,r) = quotRem x d
