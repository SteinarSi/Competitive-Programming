import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (find)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    [r, b] <- getLine <&> (words >>> map read)
    let (x, y) = solve r b
    [max x y, min x y]
        & map show
        & unwords
        & putStrLn

solve :: Int -> Int -> (Int,Int)
solve r b = [r `div` 2, r `div` 2 - 1 .. 2]
    & map (\x -> (x, r `div` 2 - x + 2))
    & find (\(x,y) -> (x-2) * (y-2) == b)
    & fromJust
