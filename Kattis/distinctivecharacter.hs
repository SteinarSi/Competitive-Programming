import           Control.Arrow         (first, second, (&&&), (***), (>>>))
import           Control.Monad         (filterM, forM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Bits             (popCount, shiftL, xor)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import qualified Data.IntSet           as S
import           Data.Maybe            (fromJust)
import           Numeric               (readBin)
import           Text.Printf           (printf)

main :: IO ()
main = do
    _:k':xs' <- C.getContents <&> C.words

    let k = readInt k'
        xs = map (C.unpack >>> readBin >>> head >>> fst) xs'
        seen = S.fromList xs

    printf ("%." <> show k <> "b\n") (tryAgain k xs)

tryAgain :: Int -> [Int] -> Int
tryAgain k xs = runST $ do
    seen <- newArray (0,m) False
    mapM_ (flip (writeArray seen) True) xs
    bfs seen xs
  where
    m = (1 `shiftL` k) - 1

    bfs :: STUArray s Int Bool -> [Int] -> ST s Int
    bfs seen curr = do
        ys <- forM curr (\x -> map (xor x) edges
            & filterM (\y -> readArray seen y >>= \r -> if r then pure False else writeArray seen y True >> pure True))
            <&> concat
        if null ys
            then pure (head curr)
            else bfs seen ys

    edges :: [Int]
    edges = map (\i -> 1 `shiftL` i) [0..k-1]

solve :: Int -> [Int] -> Int -> Int -> [Int] -> Int
solve k xs b c [] = c
solve k xs b c (y:ys) = solve' 0 xs
  where
    solve' p _ | p > b = solve k xs b c ys
    solve' p [] = solve k xs p y ys
    solve' p (z:zs) = solve' (max p (k - popCount (y `xor` z))) zs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
