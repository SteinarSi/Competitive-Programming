import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, bounds, inRange, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt, intToDigit)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> parse
        >>> map solve
        >>> unlines
        >>> putStr
    )

solve :: UArray (Int,Int) Int -> String
solve grid = [1..h]
    & map (\y -> map (\x -> if S.member (y,x) fracture then ' ' else intToDigit (grid ! (y,x))) [1..w])
    & unlines
  where
    rng@(_,(h,w)) = bounds grid
    fracture = S.fromList (runST $ do
        dist <- newArray rng maxBound
        let start = map (\x -> (grid ! (1,x),(1,x))) [1..w]
        mapM_ (\(d,u) -> writeArray dist u d) start
        search dist (S.fromList start))

    search :: STUArray s (Int,Int) Int -> S.Set (Int,(Int,Int)) -> ST s [(Int,Int)]
    search dist queue = do
        let ((d,u),queue') = S.deleteFindMin queue
        if fst u == h
            then backtrack dist u d
            else neighbours u
                & map (\(y,x) -> (d + grid ! (y,x), (y,x)))
                & filterM (\(d',v) -> readArray dist v >>= \d'' -> if d' < d'' then writeArray dist v d' >> pure True else pure False)
                >>= (foldr S.insert queue' >>> search dist)

    backtrack :: STUArray s (Int,Int) Int -> (Int,Int) -> Int -> ST s [(Int,Int)]
    backtrack dist u d
        | fst u == 1 = pure [u]
        | otherwise = do
            (v,d') <- neighbours u
                & mapM (\v -> (v,) <$> readArray dist v)
                <&> (filter (snd >>> (==(d - grid ! u))) >>> head)
            (u:) <$> backtrack dist v d'

    neighbours :: (Int,Int) -> [(Int,Int)]
    neighbours (y,x) = filter (inRange rng) [(y-1,x),(y-1,x-1),(y,x-1),(y+1,x-1),(y+1,x),(y+1,x+1),(y,x+1),(y-1,x+1)]

parse :: [C.ByteString] -> [UArray (Int,Int) Int]
parse []    = []
parse [_,_] = []
parse (h':w':xs) = splitAt h xs
    & (concatMap (C.unpack >>> map digitToInt) >>> listArray ((1,1),(h,w))) *** parse
    & uncurry (:)
  where
    h = readInt h'
    w = readInt w'

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
