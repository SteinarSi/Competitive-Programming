import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words >>> map readInt >>> solve)
        >>> C.unlines
        >>> C.putStr
    )

solve :: [Int] -> C.ByteString
solve [a,b] = runST $ do
    seen <- newArray ((0,0),(100,100)) False
    bfs seen [(a,b)] <&> (show >>> C.pack)

bfs :: STUArray s (Int,Int) Bool -> [(Int,Int)] -> ST s Int
bfs seen [] = pure (-1)
bfs seen xs | (0,0) `elem` xs = pure 0
            | otherwise = xs
        & concatMap (\(h,t) -> filter (inRange ((0,0),(100,100))) [(h,t-1+2),(h-2,t),(h+1,t-2)])
        & filterM (\ix -> readArray seen ix >>= \r -> if r then pure False else writeArray seen ix True >> pure True)
        >>= bfs seen <&> succ

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
