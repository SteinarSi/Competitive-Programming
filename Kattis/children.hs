import           Control.Arrow ((>>>))
import           Data.Bits     (shiftR)
import           Data.Functor  ((<&>))
import           Data.List     (foldl')

main :: IO ()
main = do
    [n,k] <- getLine <&> (words >>> map read)
    let (q,r) = quotRem n k
    print $ if r /= 0
        then 0
        else (fac n * inv (pow (fac k) q * fac q)) `mod` modulo

fac :: Integer -> Integer
fac = enumFromTo 1 >>> foldl' ((*) >>> (>>> (`mod` modulo))) 1

inv :: Integer -> Integer
inv a
    | i < 0 = i + modulo
    | otherwise = i
  where
    (i, _, _) = gcdExt a modulo

gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b = (t, s - q * t, g)
  where
    (q, r) = a `quotRem` b
    (s, t, g) = gcdExt b r

pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow x n
    | odd n     = (u'*x) `mod` modulo
    | otherwise = u'
  where
    u = pow x (n `shiftR` 1)
    u' = (u*u) `mod` modulo

modulo :: Integer
modulo = 1000000007
