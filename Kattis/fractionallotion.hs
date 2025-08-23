import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (forM_, when)
import           Data.Array.Base       (UArray, newListArray, readArray,
                                        writeArray, (!))
import           Data.Array.ST         (runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import           Data.List             (group)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.takeWhileEnd isDigit >>> readInt >>> solve >>> show)
        >>> unlines
        >>> putStr
    )

limit :: Int
limit = 10000

solve :: Int -> Int
solve = fundamental >>> map (\(_,a) -> 2*a+1) >>> product >>> succ >>> (`div`2)

fundamental :: Int -> [(Int,Int)]
fundamental = primefactors >>> group >>> map (head &&& length)

primefactors :: Int -> [Int]
primefactors n | n <= 1    = []
               | otherwise = smallest ! n : primefactors (n `div` (smallest ! n))

smallest :: UArray Int Int
smallest = runSTUArray $ do
    arr <- newListArray (2,limit) [2..limit]
    forM_ [2..limit] $ \k -> do
        k' <- readArray arr k
        when (k == k') (mapM_ (\n -> readArray arr n >>= (min k >>> writeArray arr n)) [k*k,k*(k+1)..limit])
    pure arr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
