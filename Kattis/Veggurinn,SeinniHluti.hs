import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let r = solve xs

    print r
    putStrLn $ if r > n `div` 2
        then "The Mexicans took our jobs! Sad!"
        else "The Mexicans are Lazy! Sad!"

solve :: [(Int,Int)] -> Int
solve = foldl overlaps [] >>> map (\(a,b) -> b+1-a) >>> sum

overlaps :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
overlaps [] (a,b) = [(a,b)]
overlaps ((p,q):xs) (a,b)
    | a <= p && q <= b = overlaps xs (a,b)
    | a <= p && p <= b+1 = overlaps xs (a,q)
    | a <= p = (p,q) : overlaps xs (a,b)
    | p <= a && b <= q = (p,q) : xs
    | a <= q+1 && q <= b = overlaps xs (p,b)
    | otherwise = (p,q) : overlaps xs (a,b)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
