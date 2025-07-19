import           Control.Arrow         ((>>>))
import           Data.Array            (Array, range)
import           Data.Array.Base       (UArray, bounds, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)
import           Data.Ord              (Down (Down))

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)
    sortOn Down xs
        & map (fromIntegral >>> (/100))
        & listArray (1,n)
        & sol n
        & print

sol :: Int -> UArray Int Double -> Double
sol n xs = maximum (map expected [0..n])
  where
    rng = ((0,0),(n,n))

    expected :: Int -> Double
    expected s = sum [ index a s * dp ! (a,s) | a <- [0..s]]

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

index :: Int -> Int -> Double
index 0 _ = 0
index a s = fromIntegral a ** (fromIntegral a / fromIntegral s)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
