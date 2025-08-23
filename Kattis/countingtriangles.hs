import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, when)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n <- fmap readInt B.getLine
    when (n/=0) $ do
        replicateM n B.getLine >>= (
                    map (B.words
                >>> map readDouble
                >>> (\(a:b:c:d:_) -> ((a,b),(c,d))))
                >>> triangles
                >>> print
            )
        main

type Point = (Double, Double)
type Segment  = (Point , Point )
data Orientation = Clockwise | Counterclockwise | Colinear
    deriving (Eq, Show)

triangles :: [Segment] -> Int
triangles [] = 0
triangles (ab:xs) = triangles' xs + triangles xs
    where
        triangles' [] = 0
        triangles' (cd:rest) = length [ () | ab `intersect` cd, ef <- rest, ab `intersect` ef, cd `intersect` ef] + triangles' rest

intersect :: Segment -> Segment -> Bool
intersect (a,b) (c,d) = or [
        o1 /= o2 && o3 /= o4,
        o1 == Colinear && onSegment a c b,
        o2 == Colinear && onSegment a d b,
        o3 == Colinear && onSegment c a d,
        o4 == Colinear && onSegment c b d
    ]
    where
        o1 = orientation a b c
        o2 = orientation a b d
        o3 = orientation c d a
        o4 = orientation c d b

onSegment :: Point -> Point -> Point -> Bool
onSegment (px, py) (qx, qy) (rx, ry) = and [
        qx <= max px rx,
        qx >= min px rx,
        qy <= max py ry,
        qy >= min py ry
    ]

orientation :: Point -> Point -> Point -> Orientation
orientation (px, py) (qx, qy) (rx, ry) | val > 0 = Clockwise
                                       | val < 0 = Counterclockwise
                                       | otherwise = Colinear
    where val = (qy-py) * (rx-qx) - (qx-px) * (ry-qy)

readInt :: B.ByteString -> Int
readInt = B.readInt >>> fromJust >>> fst

readDouble :: B.ByteString -> Double
readDouble s | B.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where Just (int, r1) = B.readInt s
          Just (dec, r2) = B.readInt (B.tail r1)
          float | B.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (B.length r1 - B.length r2 - 1))
