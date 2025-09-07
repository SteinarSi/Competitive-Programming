import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (filterM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, newArray, readArray,
                                        writeArray)
import           Data.Array.Unboxed    (UArray, assocs, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find, partition)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq

main :: IO ()
main = do
    w':h':xs <- C.getContents <&> (C.unpack >>> words)

    let rng = ((1,1),(read h',read w'))
        grid = listArray rng (concat xs)
        Just (s,_) = find (snd >>> (=='*')) (assocs grid)

    putStrLn $ runST $ do
        seen <- newArray rng False
        writeArray seen s True
        search grid seen (Seq.singleton (0,s)) <&> maybe "NOT POSSIBLE" show

search :: UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> Seq (Int,(Int,Int)) -> ST s (Maybe Int)
search grid seen Empty = pure Nothing
search grid seen ((d,(y,x)) :<| xs)
    | grid ! (y,x) == 'E' = pure (Just d)
    | otherwise = do
        (ys,zs) <- [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]
            & filter ((grid!) >>> (/='#'))
            & filterM (\v -> readArray seen v >>= \s -> unless s (writeArray seen v True) >> pure (not s))
            <&> (partition ((grid!) >>> (/='D')) >>> (map (d,) >>> Seq.fromList) *** (map (d+1,) >>> Seq.fromList))
        search grid seen (ys <> xs <> zs)
