import           Control.Arrow ((>>>))
import           Data.Array    (Array, listArray, range, (!))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Text.Printf   (printf)

main :: IO ()
main = do
    [n,k] <- getContents <&> (words >>> map read)

    let rng = ((1,0),(n,k-1))

        dp :: Array (Int,Int) Double
        dp = listArray rng (map f (range rng))

        f :: (Int,Int) -> Double
        f (r,0) = fromIntegral r
        f (r,l) = map ((,l-1) >>> (dp!)) [1..n]
                & sum
                & (/ fromIntegral n)
                & max (fromIntegral r)

    [1..n]
        & map ((,k-1) >>> (dp!))
        & sum
        & (/ fromIntegral n)
        & printf "%.7f\n"
