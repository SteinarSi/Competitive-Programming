import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b] <- getLine <&> (words >>> map read)
    divisors a
        & map (subtract b >>> abs)
        & minimum
        & print

divisors :: Int -> [Int]
divisors x = divs [] [1..]
  where
    divs qs (d:ds)
        | d*d > x   = qs
        | r /= 0    = divs qs ds
        | d == q    = q : qs
        | otherwise = d : divs (q:qs) ds
      where
        (q,r) = quotRem x d
