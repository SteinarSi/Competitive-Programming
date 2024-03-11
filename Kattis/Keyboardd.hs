import           Control.Arrow    ((>>>))
import           Control.Monad    (filterM)
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (STUArray, newArray, readArray, writeArray)
import           Data.Char        (ord)

main :: IO ()
main = do
    s <- getLine
    t <- getLine
    putStrLn $ runST $ newArray (0,26) False >>= solve s t

solve :: String -> String -> STUArray s Int Bool -> ST s String
solve [] [] sticky = filterM (index >>> readArray sticky) (' ' : ['a'..'z'])
solve [] (a:as) sticky = writeArray sticky (index a) True >> solve [] as sticky
solve (x:xs) (a:as) sticky | x == a    = solve xs as sticky
                           | otherwise = writeArray sticky (index a) True >> solve (x:xs) as sticky

index :: Char -> Int
index ' ' = 26
index  c  = ord c - ord 'a'
