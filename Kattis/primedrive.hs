import           Control.Arrow      ((>>>))
import           Control.Monad      (forM_, when)
import           Data.Array.ST      (newArray, readArray, runSTUArray,
                                     writeArray)
import           Data.Array.Unboxed (UArray, (!))
import           Text.Printf        (printf)

main :: IO ()
main = getContents >>= (
            lines
        >>> init
        >>> map (\(a:b:c:_:p) -> wait ([a,b,c],read p))
        >>> unlines
        >>> putStr
    )

wait :: (String,Int) -> String
wait ([a,b,c], p)
    | prime ! p = a:b:c:' ': printf "%.4d" p
    | p /= limit = wait ([a,b,c], succ p)
    | c /= 'Z' = wait ([a,b,succ c], 0)
    | b /= 'Z' = wait ([a,succ b, 'A'], 0)
    | a /= 'Z' = wait ([succ a, 'A', 'A'], 0)
    | otherwise = error "bruh"

prime :: UArray Int Bool
prime = runSTUArray $ do
    arr <- newArray (0,limit) True
    writeArray arr 0 False
    writeArray arr 1 False
    forM_ [2..limit] $ \k -> do
        p <- readArray arr k
        when p (forM_ [k*k,k*(k+1)..limit] (flip (writeArray arr) False))
    pure arr

limit :: Int
limit = 9999
