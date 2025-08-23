import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (sort)

main :: IO ()
main = do
    [n,m] <- getLine <&> (words >>> map read)
    eat m (m-1) [] [1..n]
        & flip zip [1..]
        & sort
        & map (snd >>> show)
        & unwords
        & putStrLn

eat :: Int -> Int -> [Int] -> [Int] -> [Int]
eat _ _ [] []     = []
eat m i ys []     = eat m i [] (reverse ys)
eat m 0 ys (x:xs) = x : eat m (m-1) ys xs
eat m i ys (x:xs) = eat m (i-1) (x:ys) xs
