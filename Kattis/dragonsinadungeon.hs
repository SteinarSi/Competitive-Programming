import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, inRange, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, assocs, bounds, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    r:c:xs <- C.getContents <&> C.words
    let rng = ((1,1),(readInt r, readInt c))
        grid = listArray rng (concatMap C.unpack xs)
        Just (s,_) = find (snd >>> (=='G')) (assocs grid)
    putStrLn $ runST $ do
        seen <- newArray rng False
        solve grid seen [s] <&> bool "NO" "YES"

solve :: UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> [(Int,Int)] -> ST s Bool
solve grid seen [] = pure False
solve grid seen xs
    | any ((grid!) >>> (=='E')) xs = pure True
    | otherwise = xs
        & concatMap (\(y,x) -> [(y-1,x),(y-1,x+1),(y,x+1),(y+1,x+1),(y+1,x),(y+1,x-1),(y,x-1),(y-1,x-1)])
        & filter (inRange (bounds grid))
        & filter ((grid!) >>> (/='M'))
        & filterM (\v -> readArray seen v >>= \r -> unless r (writeArray seen v True) >> pure (not r))
        >>= solve grid seen

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
