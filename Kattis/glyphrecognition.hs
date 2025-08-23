import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    points <- C.getContents <&> (
            C.lines
        >>> tail
        >>> map (C.words
            >>> map (readInt >>> fromIntegral)
            >>> head &&& last))

    let (val, k) = [3..8]
            & map (score points &&& id)
            & maximum

    putStrLn (show k <> " " <> printf "%.15f\n" val)

type Point = (Double,Double)
type Segment = (Point,Point)
data Orientation = Clockwise | Counterclockwise | Colinear
    deriving (Eq, Show)

score :: [Point] -> Int -> Double
score points k = findInnerArea steps mini maxi / findOuterArea steps mini maxi
    where
        lines = polygon k
        (mini, maxi) = map (dist (0,0)) points
            & (minimum >>> (0.95*))
                &&&
              (maximum >>> (2*))
        steps = 21

        findOuterArea :: Int -> Double -> Double -> Double
        findOuterArea i lo hi
                | i == 0                 = shoelace xs
                | all (inside xs) points = findOuterArea (i-1) lo mid
                | otherwise              = findOuterArea (i-1) mid hi
            where
                xs = scale mid lines
                mid = (lo + hi) / 2

        findInnerArea :: Int -> Double -> Double -> Double
        findInnerArea i lo hi
                | i == 0                 = shoelace xs
                | any (inside xs) points = findInnerArea (i-1) lo mid
                | otherwise              = findInnerArea (i-1) mid hi
            where
                xs = scale mid lines
                mid = (lo + hi) / 2

polygon :: Int -> [Segment]
polygon k = [ delta, 2*delta .. 2*pi ]
        & map (cos &&& sin)
        & segments
    where
        delta  = 2*pi / fromIntegral k

        segments :: [Point] -> [Segment]
        segments (x:xs) = zip (x:xs) (xs ++ [x])

scale :: Double -> [Segment] -> [Segment]
scale s = map (((s*) *** (s*)) *** ((s*) *** (s*)))

inside :: [Segment] -> Point -> Bool
inside lines point = map (intersect ((0,0),point)) lines
        & filter id
        & length
        & even

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
        orientation (px, py) (qx, qy) (rx, ry)
                | val > 0   = Clockwise
                | val < 0   = Counterclockwise
                | otherwise = Colinear
            where val = (qy-py) * (rx-qx) - (qx-px) * (ry-qy)

shoelace :: [Segment] -> Double
shoelace = map (\((x1,y1),(x2,y2)) -> (y1+y2) * (x1-x2))
        >>> sum
        >>> abs
        >>> (/2)

dist :: Point -> Point -> Double
dist (x,y) (a,b) = sqrt ((x-a)^^2 + (y-b)^^2)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
