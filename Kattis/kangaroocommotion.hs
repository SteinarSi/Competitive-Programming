import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, guard)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, assocs, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (intToDigit)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)
import           Data.Maybe            (fromJust)

ts :: Int
ts = 7

main :: IO ()
main = do
    rck:xs <- C.getContents <&> C.lines

    let [r,c,k] = map readInt (C.words rck)
        grid = listArray ((1,1),(r,c)) (concatMap C.unpack xs)
        Just (s,_) = find (snd >>> (=='0')) (assocs grid)
        Just (t,_) = find (snd >>> (==intToDigit (k+1))) (assocs grid)
        ans = runST $ do
            seen <- newArray (((1,1),(-ts,-ts),0),((r,c),(ts,ts),k+1)) False
            writeArray seen (s,(0,0),0) True
            solve t (r,c,k) grid seen [(s,(0,0),0)]

    putStrLn (maybe "impossible" show ans)

solve :: (Int,Int) -> (Int,Int,Int) -> UArray (Int,Int) Char -> STUArray s ((Int,Int),(Int,Int),Int) Bool -> [((Int,Int),(Int,Int),Int)] -> ST s (Maybe Int)
solve _ _ _ _ [] = pure Nothing
solve t rck@(r,c,k) grid seen xs = do
    d <- readArray seen (t,(0,0),k+1)
    if d
        then pure (Just 0)
        else concatMap neighbours xs
            & filterM (\x -> readArray seen x >>= bool (writeArray seen x True >> pure True) (pure False))
            >>= solve t rck grid seen <&> fmap succ
  where
    neighbours :: ((Int,Int),(Int,Int),Int) -> [((Int,Int),(Int,Int),Int)]
    neighbours ((y,x),(dy,dx),κ) = do
        dy' <- [max (dy-1) (-ts) .. min (dy+1) ts]
        dx' <- [max (dx-1) (-ts) .. min (dx+1) ts]
        let y' = y+dy'
            x' = x+dx'
            κ' | grid ! (y',x') == intToDigit (κ+1) = κ+1
               | otherwise                          = κ
        guard (1 <= y' && y' <= r && 1 <= x' && x' <= c)
        guard (grid ! (y',x') /= '#')
        [((y',x'),(dy',dx'),κ')]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
