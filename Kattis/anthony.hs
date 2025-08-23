import           Control.Arrow         ((>>>))
import           Data.Array            (listArray, range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n':m':rest <- C.getContents <&> C.words
    let n = readInt n'
        m = readInt m'
        xs = listArray (0,n+m-2) (map readDouble rest)
        rng = ((0,0),(n,m))
        dp = listArray rng (map chance (range rng))

        chance :: (Int,Int) -> Double
        chance (0,_) = 0
        chance (_,0) = 1
        chance (a,c) = let p = xs ! (n+m-a-c)
                       in  p * (dp ! (a,c-1)) + (1-p) * (dp ! (a-1,c))

    print (dp ! (n,m))

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
