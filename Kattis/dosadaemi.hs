{-# LANGUAGE OverloadedRecordDot #-}

import           Control.Arrow ((>>>))
import           Data.Function (on)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [me,friend,road1,road2] <- getContents <&> (lines >>> map (words >>> map read >>> \[x,y] -> Point2D x y))

    let

        projectMe = project (road1,road2) me
        projectFriend = project (road1,road2) friend

        bin :: Int -> Double -> Double -> Double
        bin i lo hi
            | i == 0    = cost ((lo + hi) / 2)
            | otherwise = case (compare `on` cost) mi1 mi2 of
                LT -> bin (i-1) lo mi2
                GT -> bin (i-1) mi1 hi
                EQ -> bin (i-1) mi1 mi2
          where
            l = (hi - lo) / 3
            mi1 = lo + l
            mi2 = hi - l

        cost :: Double -> Double
        cost c = distance me pickup + distance pickup friend
          where
            pickup = projectMe + c `scale` (projectFriend-projectMe)

    print (bin 100 0 1)

type Line p a = (p a, p a)

project :: Floating a => Line Point2D a -> Point2D a -> Point2D a
project (f,t) p = f + ((v `dot` d) / (d `dot` d)) `scale` d
  where
    d = t - f
    v = p - f

data Point2D a = Point2D { x :: a, y :: a }

dot :: Num a => Point2D a -> Point2D a -> a
dot a b = a.x * b.x + a.y * b.y

distance :: Floating a => Point2D a -> Point2D a -> a
distance a b = sqrt ((a.x-b.x)^2 + (a.y-b.y)^2)

scale :: Num a => a -> Point2D a -> Point2D a
scale c p = p { x = c * p.x, y = c * p.y }

instance Num a => Num (Point2D a) where
    (+) a b = Point2D { x = a.x + b.x, y = a.y + b.y }
    (-) a b = Point2D { x = a.x - b.x, y = a.y - b.y }
    (*) a b = Point2D { x = a.x * b.x, y = a.y * b.y }
    abs p = Point2D { x = abs p.x, y = abs p.y }
    signum p = Point2D { x = signum p.x, y = signum p.y }
    fromInteger a = Point2D { x = (fromInteger a), y = (fromInteger a) }
