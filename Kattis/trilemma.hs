import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> zipWith solve [1..]
        >>> concat
        >>> putStr
    )

solve :: Int -> [Int] -> String
solve i xs@[x1,y1,x2,y2,x3,y3] = printf "Case #%d: %s %s triangle\n" i lengths right
  where
    [u1,v1,u2,v2,u3,v3] = map fromIntegral xs :: [Double]
    a = sqrt ((u1-u2)^2 + (v1-v2)^2)
    b = sqrt ((u1-u3)^2 + (v1-v3)^2)
    c = sqrt ((u2-u3)^2 + (v2-v3)^2)
    angles = [acos ((a^2+b^2-c^2)/(2*a*b)),acos ((a^2+c^2-b^2)/(2*a*c)),acos((b^2+c^2-a^2)/(2*b*c))]

    incorrect = x1==x2 && y1==y2 || x1==x3 && y1==y3 || x2==x3 && y2==y3 || a ~= b+c || b ~= a+c || c ~= a+b

    right | incorrect = "a"
          | any (~=(pi/2)) angles = "right"
          | any (>(pi/2)) angles = "obtuse"
          | otherwise = "acute"

    lengths | incorrect = "not"
            | a ~= b || a ~= c || b ~= c = "isosceles"
            | otherwise = "scalene"

infix  4 ~=
(~=) :: (Ord a, Fractional a) => a -> a -> Bool
(~=) a b = abs (a-b) <= 0.00000001

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
