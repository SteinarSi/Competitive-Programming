import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:m:c:xs <- C.getContents <&> (C.words >>> map readInt)

    let arr = listArray (1,n) xs
        cnt = take m xs
            & map (,1)
            & M.fromListWith (+)

    putStr $ case sound n m c arr cnt 1 of
        [] -> "NONE\n"
        ys -> unlines (map show ys)

sound :: Int -> Int -> Int -> UArray Int Int -> M.IntMap Int -> Int -> [Int]
sound n m c xs cnt i
    | i+m > n   = ret
    | otherwise = ret <> sound n m c xs cnt' (i+1)
  where
    cnt' = M.insertWith (+) (xs ! (i+m)) 1 $ case cnt M.! (xs!i) of
        1 -> M.delete (xs!i) cnt
        y -> M.insert (xs!i) (y-1) cnt
    ret | fst (M.findMax cnt) - fst (M.findMin cnt) <= c = [i]
        | otherwise = []

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
