import           Control.Arrow         ((>>>))
import           Data.Bits             (countTrailingZeros, shiftR)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words >>> map readInteger >>> \[p,a] -> bool "no" "yes" (powmod a p p == a && not (isPrime p)))
        >>> unlines
        >>> putStr
    )

isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | n `elem` bases = True
    | any (mod n >>> (==0)) bases = False
    | otherwise = not (any (compositeCheck n) bases)
  where
    r = countTrailingZeros (fromInteger (n-1) :: Int)
    d = (n-1) `shiftR` r

    compositeCheck :: Integer -> Integer -> Bool
    compositeCheck n a = powmod a d n /= 1 && (n-1) `notElem` take r (iterate (\x' -> (x'*x') `mod` n) (powmod a d n))

    bases :: [Integer]
    bases = [2, 3, 5, 7]

powmod :: Integer -> Integer -> Integer -> Integer
powmod _ 0 _ = 1
powmod x n m
    | odd n     = (u'*x) `mod` m
    | otherwise = u'
  where
    u = powmod x (n `shiftR` 1) m
    u' = (u*u) `mod` m

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
