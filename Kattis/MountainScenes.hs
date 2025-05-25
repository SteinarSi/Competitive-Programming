import           Control.Arrow   ((>>>))
import           Data.Array.Base (UArray, listArray, (!))
import           Data.Functor    ((<&>))
import           Data.Ix         (range)

main :: IO ()
main = do
    [n,w,h] <- getLine <&> (words >>> map read)
    print (solve n w h)

solve :: Int -> Int -> Int -> Int
solve n w h = dp (listArray rng (map (\r -> min r h + 1) (range rng))) (w-1) - boring
  where
    max_rem = min n (w*h)
    rng = (0, max_rem)
    boring = length (filter ((w*) >>> (n>=)) [0..h])

    dp :: UArray Int Int -> Int -> Int
    dp prev 0 = prev ! max_rem
    dp prev i = dp (listArray rng (map (\r -> sum (map (\u -> prev ! (r-u)) [0 .. min r h]) `mod` (10^9 + 7)) (range rng))) (i-1)
