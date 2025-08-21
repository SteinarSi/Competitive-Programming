import           Control.Arrow ((&&&), (>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = getLine >>= (
            map (=='B')
        >>> (filter id >>> length) &&& id
        >>> uncurry (solve 0)
        >>> print
    )

solve :: Int -> Int -> [Bool] -> Int
solve l r []         = 0
solve l r (True:xs)  = solve (l+1) (r-1) xs
solve l r (False:xs) = min l r + solve l r xs
