import Data.List (sort)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray, runSTArray, readArray, writeArray, newArray)
import Data.Array (Array, (!))

main :: IO ()
main = do
    n:xs <- fmap lines getContents
    let dt = map (sort . map read . words) xs
        arr = runSTArray $ newArray (1,read n) 0 >>= count dt
    putStrLn (search dt arr)

search :: [[Int]] -> Array Int Int -> String
search [] _ = error "bruh"
search ([a,b]:xs) arr | arr ! a + arr ! b >= 4 = show a ++ " " ++ show b
                      | otherwise = search xs arr

count :: [[Int]] -> STArray s Int Int -> ST s (STArray s Int Int)
count [] arr = pure arr
count ([a,b]:xs) arr = do
    readArray arr a >>= writeArray arr a . succ
    readArray arr b >>= writeArray arr b . succ
    count xs arr
