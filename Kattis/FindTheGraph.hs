import           Control.Arrow   (second, (>>>))
import           Control.Monad   (when)
import           Data.Array.Base (UArray, freeze, listArray, (!))
import           Data.Array.IO   (IOUArray, newArray, writeArray)
import           Data.Bool       (bool)
import           Data.Function   ((&))
import           Data.Functor    ((<&>))
import           System.Exit     (exitFailure)
import           System.IO       (hFlush, stdout)

main :: IO ()
main = do
    n <- getLine <&> read
    degrees <- mapM (pure >>> query) [1..n] <&> listArray (1,n)
    graph <- newArray ((1,1),(n,n)) False :: IO (IOUArray (Int,Int) Bool)
    sequence_ [ neighbours degrees u v >>= flip when (writeArray graph (u,v) True) | u <- [1..n], v <- [u+1..n]]
    ret <- freeze graph :: IO (UArray (Int,Int) Bool)
    map (\y -> unwords (map (\x -> bool "0" "1" (ret ! bool (y,x) (x,y) (x<=y))) [1..n])) [1..n]
        & ("!":)
        & unlines
        & putStr

neighbours :: UArray Int Int -> Int -> Int -> IO Bool
neighbours deg u v = query [u,v] <&> (== deg ! u + deg ! v - 2)

query :: [Int] -> IO Int
query xs = do
    putStrLn (unwords ("?" : map show (length xs : xs)))
    hFlush stdout
    r <- getLine <&> read
    if r == -1
        then exitFailure
        else pure r

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
