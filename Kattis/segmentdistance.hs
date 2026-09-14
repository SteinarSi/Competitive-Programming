import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

type Point = (Double,Double)
type Segment = (Point,Point)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words
            >>> map readNum
            >>> \[x1,y1,x2,y2,x3,y3,x4,y4] -> printf "%.2f" (segmentDistance ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4))))
        >>> unlines
        >>> putStr
    )

segmentDistance :: Segment -> Segment -> Double
segmentDistance (a,b) (c,d)
    | (a,b) `intersect` (c,d) = 0
    | otherwise               = minimum [
            segmentPointDistance (a,b) c,
            segmentPointDistance (a,b) d,
            segmentPointDistance (c,d) a,
            segmentPointDistance (c,d) b
        ]

intersect :: Segment -> Segment -> Bool
intersect (a,b) (c,d) = counterClockwise a c d /= counterClockwise b c d && counterClockwise a b c /= counterClockwise a b d

counterClockwise :: Point -> Point -> Point -> Bool
counterClockwise (ax,ay) (bx,by) (cx,cy) = (cy - ay) * (bx - ax) > (by - ay) * (cx - ax)

segmentPointDistance :: Segment -> Point -> Double
segmentPointDistance ((x1,y1),(x2,y2)) (x,y) = sqrt ((x1 + u * px - x)^2 + (y1 + u * py - y)^2)
  where
    px = x2-x1
    py = y2-y1
    u = max 0 (min 1 (((x - x1) * px + (y - y1) * py) / (px*px + py*py)))

readNum :: Num n => C.ByteString -> n
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
