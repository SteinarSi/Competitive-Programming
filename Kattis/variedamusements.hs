import           Control.Arrow         ((>>>))
import           Data.Array            (Array, listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = getContents >>= (
            words
        >>> map read
        >>> amuse
        >>> print
    )

amuse :: [Int] -> Int
amuse [n,a,b,c] = f (n,-1)
    where
        rng = ((0,1),(n,3))

        dp :: Array (Int,Int) Int
        dp = listArray rng (map f (range rng))

        f :: (Int,Int) -> Int
        f (0,_) = 1
        f (n,p) = case p of
                1 -> modulo (pickB + pickC)
                2 -> modulo (pickA + pickC)
                3 -> modulo (pickA + pickB)
                _ -> modulo (pickA + pickB + pickC)
            where
                pickA = modulo (a * (dp ! (n-1,1)))
                pickB = modulo (b * (dp ! (n-1,2)))
                pickC = modulo (c * (dp ! (n-1,3)))

modulo :: Int -> Int
modulo = (`mod` 1000000007)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
