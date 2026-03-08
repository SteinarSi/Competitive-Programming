import           Control.Arrow         ((>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)
import           Data.STRef.Strict     (STRef, modifySTRef, newSTRef, readSTRef)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> map readInt
        >>> parse
        >>> map (ripoff >>> maybe "0" show)
        >>> unlines
        >>> putStr
    )

ripoff :: (Int, Int, Int, UArray Int Int) -> Maybe Int
ripoff (n, s, t, xs) = runST $ do
    memo <- newSTRef M.empty
    play memo t 0
  where
    play :: STRef s (M.IntMap (Maybe Int)) -> Int -> Int -> ST s (Maybe Int)
    play memo r i
        | i > n  = pure (Just 0)
        | r == 0 = pure Nothing
    play memo r i = do
        let hash = t * i + r
        d <- readSTRef memo <&> M.lookup hash
        case d of
            Just  v -> pure v
            Nothing -> do
                v <- [i+1..i+s]
                    & mapM (play memo (r-1))
                    <&> (maximum >>> fmap (+xs!i))
                modifySTRef memo (M.insert hash v)
                pure v

parse :: [Int] -> [(Int, Int, Int, UArray Int Int)]
parse [0] = []
parse (n:s:t:xs) = let (ys,zs) = splitAt n xs
                   in  (n, s, t, listArray (0, n) (0:xs)) : parse zs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
