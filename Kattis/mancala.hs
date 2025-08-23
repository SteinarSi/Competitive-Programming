import           Control.Arrow         (second, (>>>))
import           Data.Array            (Array, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> mapM_ (C.words
            >>> map readInt
            >>> (\(k:t:_) -> solve k t)
            >>> C.putStrLn
        )
    )

solve :: Int -> Int -> C.ByteString
solve k t = chunksOf 10 board
    & ([k, bin]:)
    & map (map (show >>> C.pack) >>> C.unwords)
    & C.unlines
    & C.init
    where
        board = memo t
        bin = length board

memo :: Int -> [Int]
memo = (arr !)
    where
        arr :: Array Int [Int]
        arr = listArray (0, 2118) $ [] : map (\i -> next (arr ! (i-1))) [1..]

next :: [Int] -> [Int]
next board = moveCounters 1 board
    where
        bin :: Int
        bin = firstEmpty board

        moveCounters :: Int -> [Int] -> [Int]
        moveCounters i (0:xs) | i == bin = bin : xs
        moveCounters i [] | i == bin = [bin]
        moveCounters i (x:xs) = pred x : moveCounters (i+1) xs
        moveCounters x xs = error (show (bin, x, xs))

firstEmpty :: [Int] -> Int
firstEmpty []     = 1
firstEmpty (0:xs) = 1
firstEmpty (_:xs) = succ (firstEmpty xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
