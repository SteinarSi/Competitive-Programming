import           Control.Arrow         ((>>>))
import           Data.Bits             (shiftL)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getLine >>= (C.unpack >>> solve 1 0 0 >>> print)

solve :: Int -> Int -> Int -> String -> Int
solve !k2 !num1 !ret ""       = ret
solve !k2 !num1 !ret ('0':xs) = solve k2 num1 (m (ret + num1)) xs
solve !k2 !num1 !ret ('1':xs) = solve k2 (m (num1 + k2)) ret xs
solve !k2 !num1 !ret ('?':xs) = solve (m (k2 `shiftL` 1)) (m (num1 `shiftL` 1 + k2)) (m (ret `shiftL` 1 + num1)) xs

m :: Integral a => a -> a
m = (`mod` 1000000007)
