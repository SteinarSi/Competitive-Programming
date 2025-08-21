import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (readInteger >>> (dp!) >>> snd)
        >>> unlines
        >>> putStr
    )

dp :: Array Integer (Integer,String)
dp = listArray (1,1000) (map f [1..1000])
  where
    f :: Integer -> (Integer,String)
    f 1 = (2,"2")
    f n = let (c,s) = dp ! (n-1)
              Just p = find ((c*) >>> show >>> length >>> fromIntegral >>> (==n)) primes
          in  (p*c,show p <> " " <> s)

primes :: [Integer]
primes = [2,3,5,7,11]

readInteger :: C.ByteString -> Integer
readInteger = C.readInteger >>> fromJust >>> fst
