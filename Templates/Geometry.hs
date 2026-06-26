{-# LANGUAGE OverloadedRecordDot #-}

import           Control.Arrow ((>>>))

type Line p a = (p a, p a)

class Point p where
    dot :: Num a => p a -> p a -> a
    distance :: Floating a => p a -> p a -> a
    distanceToLine :: Floating a => Line p a -> p a -> a
    scale :: Num a => a -> p a -> p a

magnitude :: (Num (p a), Point p, Floating a) => p a -> a
magnitude p = distance p 0

data Point2D a = Point2D { x :: a, y :: a }
    deriving (Show,Eq)

instance Point Point2D where
    dot :: Num a => Point2D a -> Point2D a -> a
    dot a b = a.x * b.x + a.y * b.y

    distance :: Floating a => Point2D a -> Point2D a -> a
    distance = squareDistance >>> (>>> sqrt)

    distanceToLine :: Floating a => Line Point2D a -> Point2D a -> a
    distanceToLine (f,t) p = abs ((t.y-f.y)*p.x - (t.x-f.x)*p.y + t.x*f.y - t.y*f.x) / distance f t

    scale :: Num a => a -> Point2D a -> Point2D a
    scale c p = p { x = c * p.x, y = c * p.y }

squareDistance :: Num a => Point2D a -> Point2D a -> a
squareDistance a b = (a.x-b.x)^2 + (a.y-b.y)^2

rotateLeft :: Num a => Point2D a -> Point2D a
rotateLeft p = Point2D { x = -p.y, y = p.x }

rotateRight :: Num a => Point2D a -> Point2D a
rotateRight p = Point2D { x = p.y, y = -p.x }

cross :: Num a => Point2D a -> Point2D a -> a
cross a b = a.x * b.y - a.y * b.x

isLeftOf :: (Num a, Ord a) => Point2D a -> Point2D a -> Bool
isLeftOf a b = cross a b < 0

isRightOf :: (Num a, Ord a) => Point2D a -> Point2D a -> Bool
isRightOf a b = cross a b > 0

isOrthogonalTo :: (Num a, Eq a) => Point2D a -> Point2D a -> Bool
isOrthogonalTo a b = abs (cross a b) == 1

isOppositeTo :: (Num a, Eq a) => Point2D a -> Point2D a -> Bool
isOppositeTo a b = cross a b == 0

instance Num a => Num (Point2D a) where
    (+) a b = Point2D { x = a.x + b.x, y = a.y + b.y }
    (-) a b = Point2D { x = a.x - b.x, y = a.y - b.y }
    (*) a b = Point2D { x = a.x * b.x, y = a.y * b.y }
    abs p = Point2D { x = abs p.x, y = abs p.y }
    signum p = Point2D { x = signum p.x, y = signum p.y }
    fromInteger a = Point2D { x = (fromInteger a), y = (fromInteger a) }
