import           Control.Arrow         ((>>>))
import           Data.Bits             (countTrailingZeros, shiftR)
import qualified Data.ByteString.Char8 as C
import           Numeric               (readBin, readHex, readOct)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map solve
        >>> unlines
        >>> putStr
    )

solve :: String -> String
solve xs = show (num `div` d) <> "/" <> show (den `div` d)
  where
    num = length (filter id [
            bin && isPrime (fst (head (readBin xs))),
            oct && isPrime (fst (head (readOct xs))),
            dec && isPrime (read xs),
            isPrime (fst (head (readHex xs)))
        ])
    den = 1 + length (filter id [bin,oct,dec])
    d = gcd num den

    bin = all (<='1') xs
    oct = all (<='7') xs
    dec = all (<='9') xs

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
