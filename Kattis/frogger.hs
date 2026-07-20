import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, bounds, inRange, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> map (solve >>> maybe "The problem has no solution." (printf "The minimum number of turns is %d."))
        >>> unlines
        >>> putStr
    )

solve :: (Int, (Int,Int), (Int,Int), UArray (Int,Int) Bool) -> Maybe Int
solve (r,f,g,grid) = runST $ do
    avail <- newArray rng maxBound
    seen <- newArray rng False
    search avail seen 1 [f]
  where
    rng@(_,(n,m)) = bounds grid
    safe t (y,x)
        | odd (n-y) = grid ! (y, (x - t) `mod` (m+1))
        | otherwise = grid ! (y, (x + t) `mod` (m+1))

    search :: STUArray s (Int,Int) Int -> STUArray s (Int,Int) Bool -> Int -> [(Int,Int)] -> ST s (Maybe Int)
    search avail seen t curr
        | null curr || (t>r) = pure Nothing
        | otherwise = do
            d <- readArray seen g
            if d
                then pure (Just (t-1))
                else do
                    mapM_ (flip (writeArray seen) False) curr
                    concatMap (\(y,x) -> filter (\v -> inRange rng v && safe t v) [(y,x),(y-1,x),(y+1,x),(y,x-1),(y,x+1)]) curr
                        & filterM (\v -> readArray seen v >>= \s -> if s then pure False else writeArray seen v True >> pure True)
                        >>= filterM (\v -> readArray avail v >>= \w -> if w == maxBound then writeArray avail v (t+m) >> pure True else pure (t<=w))
                        >>= search avail seen (t+1)

parse :: [C.ByteString] -> [(Int, (Int,Int), (Int,Int), UArray (Int,Int) Bool)]
parse []          = []
parse (x:nm:g:xs) = (readInt x, (n+1,j), (0,i), grid) : parse zs
  where
    [n,m] = map readInt (C.words nm)
    (ys,f:zs) = splitAt n xs
    Just i = C.elemIndex 'G' g
    Just j = C.elemIndex 'F' f
    grid = concatMap (C.unpack >>> map (/='X')) (f:ys<>[f])
        & listArray ((0,0),(n+1,m-1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
