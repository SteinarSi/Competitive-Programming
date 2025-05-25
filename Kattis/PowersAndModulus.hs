import           Control.Arrow ((>>>))
import           Data.Bits     (shiftR)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b] <- getLine <&> (words >>> map read)

    print $ if even a
        then pow (a `div` 2) b a
        else 0

pow :: Integer -> Integer -> Integer -> Integer
pow _ 0 _ = 1
pow x n m | odd n     = (u'*x) `mod` m
          | otherwise = u'
    where
        u = pow x (n `shiftR` 1) m
        u' = (u*u) `mod` m
