{-# LANGUAGE BangPatterns #-}

import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)
import           Data.Function         (on)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map readSword
        >>> sumPairs (0,0)
        >>> solve
        >>> C.putStrLn
    )

{-# INLINE readSword #-}
readSword :: C.ByteString -> (Int,Int)
readSword s = (t+b,l+r)
    where [t,b,l,r] = map (digitToInt >>> (1-)) (C.unpack s)

solve :: (Int,Int) -> C.ByteString
solve (tb,lr) = C.unwords (map (show >>> C.pack) [c, tb-2*c, lr-2*c])
    where c = on min (`div` 2) tb lr

sumPairs :: (Int,Int) -> [(Int,Int)] -> (Int,Int)
sumPairs (!tb,!lr) []             = (tb, lr)
sumPairs (!tb,!lr) ((tb',lr'):xs) = sumPairs (tb+tb', lr+lr') xs
