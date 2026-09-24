import           Control.Arrow         ((***), (>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n':xs <- C.lines <$> C.getContents

    let n = readInt n'
        (monsters,queries) = splitAt n xs

        requirements :: UArray Int Int
        requirements = monsters
            & map (C.words >>> map readInt)
            & adventure 0 0
            & drop 1
            & listArray (1,n)

        solve :: Int -> Int
        solve x = bin 0 (n+1)
          where
            bin :: Int -> Int -> Int
            bin lo hi
                | hi - lo <= 1           = lo
                | x >= requirements ! mi = bin mi hi
                | otherwise              = bin lo mi
              where
                mi = (lo+hi) `div` 2

    queries
        & drop 1
        & map (readInt >>> solve >>> show)
        & unlines
        & putStr

adventure :: Int -> Int -> [[Int]] -> [Int]
adventure ret _     []         = [ret]
adventure ret gains ([s,b]:xs) = ret : adventure (max ret (max 0 (s-gains))) (gains+b) xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
