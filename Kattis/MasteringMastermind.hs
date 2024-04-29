import           Control.Arrow    ((>>>))
import           Control.Monad    (forM_)
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (newArray, readArray, writeArray)
import           Data.Array.ST    (STUArray)
import           Data.Char        (ord)
import           Data.Function    ((&))
import           Data.Functor     ((<&>))

main :: IO ()
main = do
    _:a:b:_ <- getLine <&> (words >>> map (map ord))
    let r = zipWith (==) a b
            & filter id
            & length
        s = subtract r $ runST $ do
            arr <- newArray (ord 'A', ord 'Z') 0
            forM_ a $ \x -> readArray arr x >>= (succ >>> writeArray arr x)
            count 0 b arr
    putStrLn (show r <> " " <> show s)

count :: Int -> [Int] -> STUArray s Int Int -> ST s Int
count ret [] _ = pure ret
count ret (x:xs) arr = do
    c <- readArray arr x
    if c > 0
        then readArray arr x >>= (pred >>> writeArray arr x) >> count (ret + 1) xs arr
        else count ret xs arr
