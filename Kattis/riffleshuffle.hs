import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (elemIndex)

main :: IO ()
main = do
    [n,k] <- getLine <&> (words >>> map read)
    let s = [1..n]
        rs = iterate (shuffle (n `div` 2)) s
        Just c = elemIndex s (drop 1 rs) <&> succ
    rs !! (k `mod` c)
        & map show
        & unwords
        & putStrLn

shuffle :: Int -> [a] -> [a]
shuffle m xs = let (ys,zs) = splitAt m xs
               in  riffle zs ys

riffle :: [a] -> [a] -> [a]
riffle [] ys     = ys
riffle (x:xs) ys = x : riffle ys xs
