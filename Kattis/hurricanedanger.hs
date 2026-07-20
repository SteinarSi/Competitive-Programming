import           Control.Arrow         (second, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map C.words
        >>> parse
        >>> map danger
        >>> C.unlines
        >>> C.putStr
    )

danger :: ((Double,Double),(Double,Double),[(C.ByteString,(Double,Double))]) -> C.ByteString
danger (f,t,xs) = C.unwords (map fst (filter (snd >>> (==min)) distances))
  where
    distances = map (second (distance (f,t))) xs
    min = minimum (map snd distances)

distance :: ((Double,Double),(Double,Double)) -> (Double,Double) -> Double
distance ((x1,y1),(x2,y2)) (x,y) = minimum [
        sqrt ((x1-x)^2 + (y1-y)^2),
        sqrt ((x2-x)^2 + (y2-y)^2),
        abs ((y2-y1)*x - (x2-x1)*y + x2*y1 - y2*x1) / sqrt ((y2-y1)^2 + (x2-x1)^2)
    ]

parse :: [[C.ByteString]] -> [((Double,Double),(Double,Double),[(C.ByteString,(Double,Double))])]
parse [] = []
parse (p:[m]:xs) = ((x1,y1),(x2,y2),cities) : parse zs
  where
    [x1,y1,x2,y2] = map readNum p
    (ys,zs) = splitAt (readInt m) xs
    cities = map (\[n,x,y] -> (n, (readNum x, readNum y))) ys

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

readNum :: Num a => C.ByteString -> a
readNum = readInt >>> fromIntegral
