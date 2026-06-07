{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (guard)
import           Data.Array.ST         (newArray, readArray, runSTUArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, range, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words
            >>> map readInt
            >>> (\[n,a,b,c,d,x0,y0,m] -> take n (iterate (\(x,y) -> ((a*x+b) `mod` m, (c*y+d) `mod` m)) (x0,y0)))
            >>> solve)
        >>> zipWith (printf "Case #%d: %d") [(1::Int)..]
        >>> unlines
        >>> putStr
    )

solve :: [(Int,Int)] -> Int
solve xs = sum $ do
    i@(ix,iy) <- range rng
    j@(jx,jy) <- range rng
    guard (j >= i)
    let k = ((3 - (ix+jx)) `mod` 3, (3 - (iy+jy)) `mod` 3)
    guard (k >= j)
    pure $ if
        | i == j && j == k -> choose (cnt!i) 3
        | i == j           -> choose (cnt!i) 2 * cnt ! k
        | j == k           -> choose (cnt!j) 2 * cnt ! i
        | k == i           -> choose (cnt!k) 2 * cnt ! j
        | otherwise        -> cnt ! i * cnt ! j * cnt ! k
  where
    rng = ((0,0),(2,2))

    cnt :: UArray (Int,Int) Int
    cnt = runSTUArray $ do
        ret <- newArray rng 0
        mapM_ ((`mod`3) *** (`mod`3) >>> \i -> readArray ret i >>= (succ >>> writeArray ret i)) xs
        pure ret

choose :: Int -> Int -> Int
choose n k
    | k > n           = 0
    | k == 0          = 1
    | k > (n `div` 2) = n `choose` (n-k)
    | otherwise       = n * ((n-1) `choose` (k-1)) `div` k

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
