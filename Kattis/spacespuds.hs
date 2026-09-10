import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    xs <- C.getContents <&> (C.lines >>> drop 1 >>> map (C.words >>> map readInt >>> \[a,b] -> (a,b)))
    let path = last xs : xs
    print (shoelace path - sum (zipWith boundary path xs) `div` 2 + 1)

boundary :: (Int,Int) -> (Int,Int) -> Int
boundary (x,y) (a,b)
    | dx == 0 && dy == 0 = 1
    | dy == 0 = dx
    | otherwise = gcd dx dy
  where
    dx = abs (x-a)
    dy = abs (y-b)

shoelace :: [(Int, Int)] -> Int
shoelace xs = abs (sum (zipWith (\(x1,y1) (x2,y2) -> (y1+y2) * (x1-x2)) xs (drop 1 xs))) `div` 2

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
