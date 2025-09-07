{-# LANGUAGE MultiWayIf #-}

import           Control.Arrow ((&&&), (>>>))
import           Data.Functor  ((<&>))
import           Data.List     (find, permutations)
import           Data.Maybe    (fromJust)
import           Data.Ratio    ((%))

type Point = (Int,Int)

main :: IO ()
main = do
    xs@[a,b,c,d] <- getContents <&> (lines >>> map (words >>> map read >>> head &&& last) >>> order)

    let ab = b.-a
        bc = c.-b
        cd = d.-c
        da = a.-d

        allRight = and [right a b c, right b c d, right c d a, right d a b]
        parallels = [parallel ab cd, parallel bc da]
        trapezoid = or parallels
        parallelogram = and parallels
        allSameLength = sameLength ab bc && sameLength bc cd && sameLength cd da
        kite = sameLength ab bc && sameLength cd da || sameLength bc cd && sameLength da ab

    putStrLn $ if
        | parallelogram && allRight && allSameLength -> "Square"
        | parallelogram && allRight                  -> "Rectangle"
        | parallelogram && allSameLength             -> "Rhombus"
        | parallelogram                              -> "Parallelogram"
        | trapezoid                                  -> "Trapezoid"
        | kite                                       -> "Kite"
        | otherwise                                  -> "Quadrilateral"

sameLength :: Point -> Point -> Bool
sameLength (a,b) (x,y) = a^2 + b^2 == x^2 + y^2

parallel :: Point -> Point -> Bool
parallel (a,b) (x,y)
    | b==0 && y==0 = True
    | b==0 || y==0 = False
    | otherwise    = (a % b) == (x % y)

order :: [Point] -> [Point]
order = permutations >>> find (\[a,b,c,d] -> left a b c && left b c d && left c d a && left d a b) >>> fromJust

left :: Point -> Point -> Point -> Bool
left x y z = cross (x .- y) (z .- y) < 0

right :: Point -> Point -> Point -> Bool
right x y z = dot (x .- y) (z .- y) == 0

(.-) :: Point -> Point -> Point
(.-) (a,b) (x,y) = (a-x,b-y)

dot :: Point -> Point -> Int
dot (a, b) (x, y) = a*x + b*y

cross :: Point -> Point -> Int
cross (a, b) (x, y) = a*y - b * x
