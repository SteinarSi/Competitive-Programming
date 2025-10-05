import           Control.Arrow    ((>>>))
import           Control.Monad    (filterM)
import           Control.Monad.ST (ST, runST)
import           Data.Array.ST    (STUArray, inRange, newArray, readArray,
                                   writeArray)
import           Data.Bits        (shiftL)
import           Data.Bool        (bool)
import           Data.Function    ((&))
import           Data.Functor     ((<&>))

main :: IO ()
main = do
    [n,m] <- getLine <&> (words >>> map read)

    print $ runST $ do
        seen <- newArray (1,25000) False
        writeArray seen n True
        search m seen 0 [n]

search :: Int -> STUArray s Int Bool -> Int -> [Int] -> ST s Int
search m seen i xs = do
    d <- readArray seen m
    if d
        then pure i
        else concatMap (\x -> filter (inRange (1,25000)) [x-1,x+1,x `shiftL` 1]) xs
                & filterM (\y -> readArray seen y >>= bool (writeArray seen y True >> pure True) (pure False))
                >>= search m seen (i+1)
