import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, assocs, bounds,
                                        listArray, newArray, readArray,
                                        writeArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq

main :: IO ()
main = do
    (h:w:rest) <- C.getContents <&> C.words

    let grid = listArray ((1,1),(readInt h, readInt w)) (concatMap C.unpack rest)
        Just (s,_) = find (snd >>> (=='S')) (assocs grid)

    putStrLn $ runST $ do
        seen <- newArray (bounds grid) False
        solve seen grid (Seq.singleton (s,0))

solve :: STUArray s (Int,Int) Bool -> UArray (Int,Int) Char -> Seq ((Int,Int),Int) -> ST s String
solve seen grid Empty          = pure "thralatlega nettengdur"
solve seen grid (((y,x),d) :<| xs)
    | grid ! (y,x) == 'G' = pure (show d)
    | otherwise = [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]
        & filter (inRange (bounds grid))
        & filter ((grid!) >>> (/='#'))
        & filterM (\y -> readArray seen y >>= \s -> if s then pure False else writeArray seen y True >> pure True)
        >>= (map (,d+1)
            >>> Seq.fromList
            >>> (xs <>)
            >>> solve seen grid)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
