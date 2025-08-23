import           Control.Arrow         ((&&&), (>>>))
import           Data.Array            (Array, range)
import           Data.Array.Base       (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    ([k,t,p,q,x],xs) <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt) >>> head &&& (tail >>> map (map fromIntegral)))
    predictions p q x
        & zipWith probability xs
        & listArray (1,t)
        & genius k t
        & print

genius :: Int -> Int -> UArray Int Double -> Double
genius k n xs = sum [dp ! (k',n) | k' <- [k..n]]
  where
    rng = ((0,0),(n,n))

    dp :: Array (Int,Int) Double
    dp = listArray rng (map f (range rng))

    f :: (Int,Int) -> Double
    f (0,0) = 1
    f (0,s) = (1 - xs ! s) * dp ! (0,s-1)
    f (a,s) | a < 0 = 0
            | a > s = 0
            | otherwise = p * dp ! (a-1,s-1) + (1-p) * dp ! (a,s-1)
      where
        p = xs ! s

predictions :: Int -> Int -> Int -> [Int]
predictions p q = iterate ((p*) >>> (`mod` q)) >>> map (`mod` 4)

probability :: [Double] -> Int -> Double
probability [a,b,c,d] x = case x of
    0 -> chance a b c d
    1 -> chance b a c d
    2 -> chance c d a b
    3 -> chance d c a b
  where
    chance :: Double -> Double -> Double -> Double -> Double
    chance p q r s = chanc p q * (chanc p r * chanc r s + chanc p s * chanc s r)

    chanc :: Double -> Double -> Double
    chanc u v = u / (u+v)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
