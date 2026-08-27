import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (delete)

main :: IO ()
main = do
    [[m1,m2,_],xs,ys] <- getContents <&> (lines >>> map (words >>> map read))
    print $ breed (m1,m2) xs ys

breed :: (Int,Int) -> [Int] -> [Int] -> Int
breed (m1,m2) [] _ = 0
breed (m1,m2) _ [] = 0
breed (m1,m2) xs (y:ys) = case (elem n1 xs, elem n2 xs) of
    (False,False) -> breed (m1,m2) xs ys
    (True, False) -> p1
    (False,True ) -> p2
    (True, True ) -> max p1 p2
  where
    n1 = y+y-m1
    n2 = y+y-m2
    p1 = 1 + breed (m1,m2) (delete n1 xs) ys
    p2 = 1 + breed (m1,m2) (delete n2 xs) ys
