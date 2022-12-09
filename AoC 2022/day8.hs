import Data.Array (Array, array, bounds, (!))
import Data.Bool (bool)
import Data.Char (digitToInt)

main :: IO ()
main = readFile "day8-input.txt" >>= print . solve . chart . lines

solve :: Array (Int, Int) Int -> (Int, Int)
solve mapp = (visible, nicest)
    where (m,n) = snd (bounds mapp)
          high  = view (-1, 0) [2..m-1]     [2..n-1]     mapp
          low   = view ( 1, 0) [m-1,m-2..2] [2..n-1]     mapp
          left  = view ( 0,-1) [2..m-1]     [2..n-1]     mapp
          right = view ( 0, 1) [2..m-1]     [n-1,n-2..2] mapp
          nicest =  maximum (map (\(i,j) -> let a = mapp ! (i,j) in product [
                high  ! (i-1,j) !! a,
                left  ! (i,j-1) !! a,
                low   ! (i+1,j) !! a,
                right ! (i,j+1) !! a
            ]) [(i,j)|i<-[2..m-1], j<-[2..n-1]])
          visible = 2*n + (m-2)*2 + sum (map (\(i,j) -> let a = mapp ! (i,j) in bool 0 1 (
                high  ! (i-1,j) !! a == i-1 && a > mapp ! (1,j) ||
                left  ! (i,j-1) !! a == j-1 && a > mapp ! (i,1) ||
                low   ! (i+1,j) !! a == m-i && a > mapp ! (m,j) ||
                right ! (i,j+1) !! a == n-j && a > mapp ! (i,n)
            )) [(i,j)|i<-[2..m-1], j<-[2..n-1]])

chart :: [String] -> Array (Int, Int) Int
chart xs = array ((1, 1), (length xs, length (head xs))) (chartHelper 1 1 xs)
    where chartHelper _ _ [] = []
          chartHelper m n ([]:xss) = chartHelper (m+1) 1 xss
          chartHelper m n ((x:xs):xss) = ((m, n), digitToInt x) : chartHelper m (n+1) (xs:xss)

view :: (Int, Int) -> [Int] -> [Int] -> Array (Int, Int) Int -> Array (Int, Int) [Int]
view (dm, dn) ms ns mapp = ret
    where (m, n) = snd (bounds mapp)
          edges = [((i,j), replicate 10 1) | (i,j) <- ([(i,j) | i <- [1..m], j<-[1,n]]++[(i,j)|i <- [1, m], j<-[1..n]])]
          ret  = array (bounds mapp) (edges ++ [ ((i, j), map (\(t,r) -> bool (r+1) 1 (mapp!(i,j)>=t)) (zip [0..] (ret!(i+dm,j+dn)))) | i<-ms, j<-ns])
