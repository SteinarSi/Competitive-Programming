import           Control.Arrow         ((&&&), (>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (sortOn)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    start:goal:_:rest <- C.getContents
            <&> (C.lines
                >>> map (C.words
                    >>> map readDouble
                    >>> head &&& last))

    rest
        & sortOn (launch goal)
        & (goal:)
        & listArray (1, length rest + 1)
        & cannonball start
        & print

cannonball :: (Double,Double) -> Array Int (Double,Double) -> Double
cannonball start cannons = [1..n]
        & map (\c -> dist start (cannons ! c) / 5 + dp ! c)
        & minimum
    where
        n = length cannons

        dp :: Array Int Double
        dp = listArray (1,n) (map f [1..n])

        f :: Int -> Double
        f 1 = 0
        f i = [1..i-1]
                & map (\c -> launch (cannons ! i) (cannons ! c) + dp ! c)
                & minimum

launch :: (Double,Double) -> (Double,Double) -> Double
launch u v | d >= 50   = (d-50)/5 + 2
           | otherwise = min (d/5) ((50-d)/5 + 2)
    where d = dist u v

dist :: (Double,Double) -> (Double,Double) -> Double
dist (a,b) (x,y) = sqrt ((a-x)^2 + (b-y)^2)

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-'  = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise        = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise       = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
