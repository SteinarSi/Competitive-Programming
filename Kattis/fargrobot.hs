import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n, xs] <- getContents <&> lines
    reverse xs
        & count [] (1,1,1)
        & move (read n)
        & putStrLn

move :: Int -> [(Int,Int,Int)] -> String
move 0 _  = ""
move _ [] = ""
move n ((r,g,b):xs) = d : move (n-1) (drop (m-1) xs)
    where m = maximum [r,g,b]
          d | r == m = 'R'
            | g == m = 'G'
            | b == m = 'B'

count :: [(Int,Int,Int)] -> (Int,Int,Int) -> String -> [(Int,Int,Int)]
count ret (r,g,b) []       = (r,g,b) : ret
count ret (r,g,b) ('R':xs) = count ((r,g,b):ret) (1,g+1,b+1) xs
count ret (r,g,b) ('G':xs) = count ((r,g,b):ret) (r+1,1,b+1) xs
count ret (r,g,b) ('B':xs) = count ((r,g,b):ret) (r+1,g+1,1) xs
count ret (r,g,b) (_:xs)   = count ((r,g,b):ret) (r+1,g+1,b+1) xs
