import Data.Array
import Data.Bool (bool)

main = do
    inn <- readFile "day8-input.txt"
    let mapp = chart (lines inn)
    print (count mapp)


count :: Map -> Int
count mapp = 2*n + (m-2)*2 + sum (map (\ij -> bool 1 0 (all (\d -> mapp ! ij <= d ! ij) [high,low,left,right])) [(i,j)|i<-[2..m-1], j<-[2..n-1]])
    where (m,n) = snd (bounds mapp)
          high  = view (-1, 0) [2..m-1]     [2..n-1]     mapp
          low   = view ( 1, 0) [m-1,m-2..2] [2..n-1]     mapp
          left  = view ( 0,-1) [2..m-1]     [2..n-1]     mapp
          right = view ( 0, 1) [2..m-1]     [n-1,n-2..2] mapp

type Map = Array (Int, Int) Char

chart :: [String] -> Map
chart xs = array ((1, 1), (length xs, length (head xs))) (chartHelper 1 1 xs)

chartHelper :: Int -> Int -> [String] -> [((Int, Int), Char)]
chartHelper _ _ [] = []
chartHelper m n ([]:xss) = chartHelper (m+1) 1 xss
chartHelper m n ((x:xs):xss) = ((m, n), x) : chartHelper m (n+1) (xs:xss)

view :: (Int, Int) -> [Int] -> [Int] -> Map -> Map
view delta ms ns mapp = ret
    where (m, n) = snd (bounds mapp)
          edges = [((i,j), mapp ! (i,j)) | i <- [1..m], j<-[1,n]] ++ [((i,j), mapp ! (i,j)) | i <- [1, m], j<-[1..n]]
          ret = array (bounds mapp) (edges ++ [ ((i, j), max (mapp!((i,j)+++delta)) (ret!((i,j)+++delta))) | i<-ms, j<-ns])

(+++) :: Num a => (a, a) -> (a, a) -> (a, a)
(+++) (a, b) (c, d) = (a+c, b+d)

putMap :: Map -> Int -> IO ()
putMap mapp 0 = return ()
putMap mapp i = putMap mapp (i-1) >> putStrLn (map (\j -> mapp ! (i,j)) [1..snd . snd . bounds $ mapp])

