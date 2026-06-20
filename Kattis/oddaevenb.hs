import           Control.Arrow ((>>>))
import           Data.Array    (listArray, range, (!))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read

    let dp = listArray rng (map f (range rng))
        rng = ((0,'A'),(n,'B'))

        f (0,_)   = 1
        f (i,'A') = sum (map (\j -> dp ! (j,'B')) (takeWhile (>=0) [i-1,i-3..])) `mod` 1000000007
        f (i,'B') = sum (map (\j -> dp ! (j,'A')) (takeWhile (>=0) [i-2,i-4..])) `mod` 1000000007

    print ((dp ! (n,'A') + dp ! (n,'B')) `mod` 1000000007)
