import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, listArray, newArray,
                                        readArray, writeArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words
            >>> map readInt
            >>> \(n:xs) -> solve n (sort xs))
        >>> unlines
        >>> putStr
    )

solve :: Int -> [Int] -> String
solve n xs = runST $ do
    memo <- newArray ((1,0),(n,600*n)) False
    best <- solulu memo (n,0)
    pure (show best <> " " <> show (total-best))
  where
    total = psum ! n
    target = (total + 1) `div` 2

    arr :: UArray Int Int
    arr = listArray (1,n) xs

    psum :: UArray Int Int
    psum = listArray (0,n) (scanl (+) 0 xs)

    solulu :: STUArray s (Int,Int) Bool -> (Int,Int) -> ST s Int
    solulu memo (i,t)
        | t >= target           = pure t
        | i == 0                = pure maxBound
        | t + psum ! i < target = pure maxBound
        | otherwise             = do
            seen  <- readArray memo (i,t)
            if seen
                then pure maxBound
                else do
                    pick <- solulu memo (i-1,t+arr ! i)
                    skip <- solulu memo (i-1,t)
                    let ret = min pick skip
                    writeArray memo (i,t) True
                    pure ret

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
