import           Control.Arrow         (second, (>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (inRange)
import           Data.Array.Base       (STUArray, UArray, assocs, bounds, elems,
                                        listArray, newArray, readArray,
                                        writeArray, (!), (//))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..), fromList, singleton)

main :: IO ()
main = do
    r:c:xs <- C.getContents <&> C.words

    let grid = listArray ((0,0),(readInt r - 1,readInt c - 1)) (concatMap C.unpack xs)

    C.putStrLn (solve (readInt c) grid)

solve :: Int -> UArray (Int,Int) Char -> C.ByteString
solve c grid = case runST (newArray (bounds grid) (-1) >>= (`search` singleton start)) of
    Nothing -> C.pack "call for help"
    Just ps -> grid // map (,'X') ps
        & elems
        & chunksOf c
        & unlines
        & init
        & C.pack
  where
    search :: forall s. STUArray s (Int,Int) Int -> Seq (Int,Int) -> ST s (Maybe [(Int,Int)])
    search prev Empty = pure Nothing
    search prev (u@(y,x) :<| xs)
        | grid ! u == 'P' = readArray prev u >>= (unhash >>> backtrack []) <&> Just
        | otherwise       = do
            ys <- [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]
                & filter (inRange (bounds grid))
                & filter ((grid!) >>> (/='#'))
                & filterM (\v -> do
                    r <- readArray prev v
                    if r /= -1
                        then pure False
                        else writeArray prev v (hash v) >> pure True)
            mapM_ (flip (writeArray prev) (hash u)) ys
            search prev (xs <> fromList ys)
      where
        backtrack :: [(Int,Int)] -> (Int,Int) -> ST s [(Int,Int)]
        backtrack ret u | grid ! u == 'A' = pure ret
                        | otherwise       = readArray prev u >>= (unhash >>> backtrack (u:ret))

    Just (start,_) = find (snd >>> (=='A')) (assocs grid)
    hash (y,x) = c*y + x
    unhash = (`quotRem` c)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
