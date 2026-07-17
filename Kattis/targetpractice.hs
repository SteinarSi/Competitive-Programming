import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust, isNothing)

main :: IO ()
main = do
    (n,_):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    let
        p1:p2:ys = xs
        ans = n <= 4 || case single (p1,dir p1 p2) Nothing ys of
            Nothing -> True
            Just p3 -> isNothing (single (p1,dir p1 p3) (Just p2) xs)
                    || isNothing (single (p2,dir p2 p3) (Just p1) xs)

    putStrLn $ if ans
        then "success"
        else "failure"

beam :: ((Int,Int), (Int,Int)) -> Maybe (Int,Int) -> [(Int,Int)] -> Maybe ((Int,Int),(Int,Int))
beam _ _ [] = Nothing
beam pd er (p:ps)
    | hit pd p  = beam pd er ps
    | otherwise = maybe (beam pd (Just p) ps) ((p,) >>> Just) er

single :: ((Int,Int), (Int,Int)) -> Maybe (Int,Int) -> [(Int,Int)] -> Maybe (Int,Int)
single (p1,d1) err [] = Nothing
single pd1 err (p:ps)
    | hit pd1 p = single pd1 err ps
    | Just p == err = single pd1 err ps
    | otherwise     = case err of
        Nothing -> single pd1 (Just p) ps
        Just  q -> find (\r -> not (hit pd1 r) && not (hit (p, dir p q) r)) ps

hit :: ((Int,Int), (Int,Int)) -> (Int,Int) -> Bool
hit (p,d) q = p == q || (dx,dy) == d || (-dx,-dy) == d
  where
    (dx,dy) = dir p q

dir :: (Int,Int) -> (Int,Int) -> (Int,Int)
dir (x1,y1) (x2,y2)
    | x1 == x2 && y1 == y2 = (0,0)
    | otherwise = (dx `div` gcd dx dy, dy `div` gcd dx dy)
  where
    dx = x2-x1
    dy = y2-y1

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
