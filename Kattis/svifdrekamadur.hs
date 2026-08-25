import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust, isJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map readInt
        >>> zip [1..]
        >>> solve M.empty 0
        >>> print
    )

solve :: M.IntMap Int -> Int -> [(Int,Int)] -> Int
solve _ ret [] = ret
solve m ret ((i,x):xs) = solve (M.insert x i hi) (max ret (max down up)) xs
  where
    (_,y,hi) = M.splitLookup x m
    down = maybe 0 (snd >>> (i-)) (M.lookupLT x m)
    up | isJust y  = 0
       | otherwise = maybe 0 (snd >>> (i-)) (M.lookupMin hi)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
