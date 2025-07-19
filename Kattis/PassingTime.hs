import           Control.Arrow    ((&&&), (>>>))
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (STUArray)
import           Data.Array.ST    (getElems, newArray, readArray, writeArray)
import           Data.Char        (digitToInt)
import           Data.Functor     ((<&>))

main :: IO ()
main = do
    (n,xs) <- getContents <&> (lines >>> (head >>> read) &&& (tail >>> map parse))

    let m = 52-n
        b = guess xs

    print (fromIntegral b / fromIntegral m)

guess :: [Int] -> Int
guess xs = runST $ do
    ret <- newArray (1,13) 4 :: ST s (STUArray s Int Int)
    mapM_ (\x -> readArray ret x >>= (pred >>> writeArray ret x)) xs
    getElems ret <&> maximum

parse :: String -> Int
parse (x:_) = case x of
    'A' -> 1
    '1' -> 10
    'J' -> 11
    'Q' -> 12
    'K' -> 13
    _   -> digitToInt x
