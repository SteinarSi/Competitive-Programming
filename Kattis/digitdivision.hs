import           Control.Arrow ((>>>))
import           Data.Bits     (shiftR)
import           Data.Char     (digitToInt)
import           Data.Functor  ((<&>))


main :: IO ()
main = do
    [_,d] <- getLine <&> (words >>> map read)
    xs <- getLine <&> map digitToInt
    print (solve d 0 0 xs)

solve :: Int -> Int -> Int -> [Int] -> Int
solve d c p [] | p `mod` d == 0 = pow 2 (c-1) 1000000007
               | otherwise = 0
solve d c p (x:xs) = solve d c' p' xs
    where
        p' = (p * 10 + x) `mod` d
        c' | p' == 0   = c+1
           | otherwise = c

pow :: Int -> Int -> Int -> Int
pow _ 0 _ = 1
pow x n m | odd n     = (u'*x) `mod` m
          | otherwise = u'
    where
        u = pow x (n `shiftR` 1) m
        u' = (u*u) `mod` m
