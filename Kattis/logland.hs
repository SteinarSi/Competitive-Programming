import           Control.Arrow         ((>>>))
import           Data.Bits             (shiftR)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> zip [0..]
        >>> solve 0 0
        >>> print
    )

solve :: Int -> Int -> [(Int,Int)] -> Int
solve ret _ []         = ret
solve ret carry ((e,x):xs) = solve ret' ((carry + x) `div` 2) xs
    where
        ret' | carry == 0 && odd x = (ret + pow 2 e m) `mod` m
             | otherwise = ret

m :: Int
m = 1000000007

pow :: Int -> Int -> Int -> Int
pow _ 0 _ = 1
pow x n m | odd n     = (u'*x) `mod` m
          | otherwise = u'
    where
        u = pow x (n `shiftR` 1) m
        u' = (u*u) `mod` m

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
