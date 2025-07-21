import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt >>> (\[r,c,sx,sy,tx,ty] -> solve ((1,1),(r,c)) (sx,sy) (tx,ty)))
        >>> unlines
        >>> putStr
    )

solve :: ((Int,Int),(Int,Int)) -> (Int,Int) -> (Int,Int) -> String
solve rng s t = runST $ do
    seen <- newArray rng False
    writeArray seen s True
    search seen (Seq.singleton (s,0)) <&> maybe "impossible" show
  where
    search :: STUArray s (Int,Int) Bool -> Seq ((Int,Int),Int) -> ST s (Maybe Int)
    search seen Empty = pure Nothing
    search seen (((x,y),d) :<| xs) | (x,y) == t = pure (pure d)
                                   | otherwise  = jumps (x,y)
        & filter (inRange rng)
        & filterM (\v -> readArray seen v >>= \r -> if r then pure False else writeArray seen v True >> pure True)
        >>= (map (,d+1) >>> Seq.fromList >>> (xs <>) >>> search seen)

-- The compiler crashes for some reason if this function is allowed to be inlined o.O
{-# NOINLINE jumps #-}
jumps :: (Int,Int) -> [(Int,Int)]
jumps (x,y) = [(x-2,y-1),(x-1,y-2),(x+1,y-2),(x+2,y-1),(x+2,y+1),(x+1,y+2),(x-1,y+2),(x-2,y+1)]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
