import           Control.Arrow            ((>>>))
import           Data.Array               (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe               (fromJust)
import           Text.Printf              (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> mapM_ (
                C.words
            >>> map (C.readInt >>> fromJust >>> fst)
            >>> (\(k:n:_) -> solve k n)
            >>> printf "%.10f"
            >>> putStrLn
        )
    )

solve :: Int -> Int -> Double
solve k n = let tight = sum  [dp ! (i,n) | i <- [0..k]]
                total = fromIntegral (k+1)^n
            in  fromIntegral (tight * 100) / total
    where
        dp :: Array (Int,Int) Integer
        dp = listArray ((0,1),(k,n)) [ f i m | i <- [0..k], m <- [1..n] ]

        f :: Int -> Int -> Integer
        f _ 1 = 1
        f i m = lo + dp ! (i, m-1) + hi
            where 
                lo | i == 0    = 0
                   | otherwise = dp ! (i-1, m-1)
                hi | i == k    = 0
                   | otherwise = dp ! (i+1, m-1)
