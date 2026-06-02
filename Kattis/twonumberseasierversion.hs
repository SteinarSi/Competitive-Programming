import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (delete, sort)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    C.getLine
    xs <- C.getLine <&> (C.words >>> map readInt >>> sort)
    [a,m] <- C.getLine <&> (C.words >>> map readDouble)

    let avg = average xs
        med = median xs

        valid :: [Int] -> Bool
        valid xs = average xs - avg == a && median xs - med == m

        attempts :: [Int] -> [String]
        attempts []     = []
        attempts (x:ys) = filter (\y -> valid (delete y (delete x xs))) ys
            & map (printf "%d %d" x)
            & (<> attempts ys)

    attempts xs
        & unlines
        & putStr

average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

median :: [Int] -> Double
median xs
    | odd (length xs) = fromIntegral (xs !! (length xs `div` 2))
    | otherwise       = average (take 2 (drop ((n-1) `div` 2) xs))
  where
    n = length xs

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
