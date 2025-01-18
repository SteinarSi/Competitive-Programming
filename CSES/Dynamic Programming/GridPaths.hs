import           Control.Arrow   ((>>>))
import           Data.Array      (Array, range)
import           Data.Array.Base (UArray, bounds, listArray, (!))
import           Data.Functor    ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read
    grid <- getContents <&> (filter (`elem` ".*") >>> listArray ((1,1),(n,n)))
    print (solve n grid)

solve :: Int -> UArray (Int,Int) Char -> Int
solve n grid = dp ! (n,n)
    where
        dp :: Array (Int,Int) Int
        dp = listArray (bounds grid) (map f (range (bounds grid)))

        f :: (Int,Int) -> Int
        f ij | grid ! ij == '*' = 0
        f (1,1) = 1
        f (1,x) = dp ! (1,x-1)
        f (y,1) = dp ! (y-1,1)
        f (y,x) = (dp ! (y-1,x) + dp ! (y,x-1)) `mod` 1000000007
