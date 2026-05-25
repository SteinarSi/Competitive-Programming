import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt)
        >>> solve M.empty M.empty
        >>> map (show >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

solve :: M.IntMap Int -> M.IntMap Int -> [[Int]] -> [Int]
solve as bs [] = []
solve as bs ([a,b]:xs) = maximum (mali (M.toAscList as') (M.toDescList bs')) : solve as' bs' xs
  where
    as' = M.insertWith (+) a 1 as
    bs' = M.insertWith (+) b 1 bs

mali :: [(Int,Int)] -> [(Int,Int)] -> [Int]
mali [] [] = []
mali ((a,ac):as) ((b,bc):bs) = a+b : case compare ac bc of
    LT -> mali as ((b,bc-ac):bs)
    EQ -> mali as bs
    GT -> mali ((a,ac-bc):as) bs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
