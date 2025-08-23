import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:k:xs <- C.getContents <&> (C.words >>> map readInt)

    let m = bags (0, Nothing, Nothing, Nothing) xs

    print (solve m k)

solve :: (Int, Maybe Int, Maybe Int, Maybe Int) -> Int -> Int
solve m@(m0, m1, m2, m3) k = case k `mod` 4 of
    0 -> m0 + k
    1 -> eat m1
    2 -> eat m2
    3 -> eat m3
  where
    eat = maybe k ((k+) >>> solve m)

bags :: (Int, Maybe Int, Maybe Int, Maybe Int) -> [Int] -> (Int, Maybe Int, Maybe Int, Maybe Int)
bags m             []     = m
bags (m0,m1,m2,m3) (x:xs) = case x `mod` 4 of
    0 -> bags (m0+x,m1,m2,m3) xs
    1 -> bags (m0, max m1 (Just x), m2, m3) xs
    2 -> bags (m0, m1, max m2 (Just x), m3) xs
    3 -> bags (m0, m1, m2, max m3 (Just x)) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
