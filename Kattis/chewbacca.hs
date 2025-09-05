import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n,k,_]:xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let ks = map (k^) [0..]
            & scanl1 (+)
            & takeWhile (<=k*n)

        depth :: Int -> (Int,Int)
        depth u = (l,u)
          where
            Just l = find ((layer!) >>> (>=u)) [0..]

        layer :: UArray Int Int
        layer = listArray (0,length ks) (0: ks)

        prev :: (Int,Int) -> (Int,Int)
        prev (l,u) = (l-1, 1 + layer ! (l-2) + (u - layer ! (l-1) - 1) `div` k)

        lca :: (Int,Int) -> (Int,Int) -> Int
        lca (l1,u) (l2,v) = case compare l1 l2 of
            LT -> 1 + lca (l1,u) (prev (l2,v))
            GT -> 1 + lca (prev (l1,u)) (l2,v)
            EQ | u == v    -> 0
               | otherwise -> 2 + lca (prev (l1,u)) (prev (l2,v))

    xs
        & map (\[x,y] -> show (lca (depth x) (depth y)))
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
