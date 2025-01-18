import           Control.Arrow    ((>>>))
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (STUArray, newArray, readArray, writeArray)
import           Data.Function    ((&))
import           Data.Functor     ((<&>))

main :: IO ()
main = do
    n <- getContents <&> read
    print $ runST $ do
        dp <- newArray (0,1000000) (-1)
        solve dp n

solve :: STUArray s Int Int -> Int -> ST s Int
solve dp 0 = pure 0
solve dp x | x < 0 = pure maxBound
           | x < 10 = pure 1
           | otherwise = do
                p <- readArray dp x
                if p /= -1
                    then pure p
                    else do
                        q <- digits x
                            & filter (>0)
                            & mapM ((x-) >>> solve dp)
                            <&> (minimum >>> succ)
                        writeArray dp x q
                        pure q

digits :: Int -> [Int]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)
