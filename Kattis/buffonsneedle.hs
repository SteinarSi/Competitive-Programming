import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readDouble))

    let int = filter (\[x1,y1,x2,y2] -> floor x1 /= floor x2) xs
        x = fromIntegral (length (filter (\[x1,y1,x2,y2] -> floor x1 /= floor x2) xs)) / n

    print (2 / x)

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
