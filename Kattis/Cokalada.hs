import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (find)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    k <- getLine <&> read
    let n = map (2^) [0..]
            & find (>=k)
            & fromJust
        p | n == k    = 0
          | otherwise = bin 0 k 1 n
    [n, p]
        & map show
        & unwords
        & putStrLn

bin :: Int -> Int -> Int -> Int -> Int
bin c k lo hi = case compare mid k of
        EQ -> c+1
        LT -> bin (c+1) k (mid+1) hi
        GT -> bin (c+1) k lo (mid-1)
    where mid = (lo + hi) `div` 2
