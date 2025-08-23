import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad         (filterM, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, bounds, listArray,
                                        newArray, readArray, writeArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)
import           Data.List             (partition)
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..), fromList, singleton)

main :: IO ()
main = do
    rc:rest <- C.getContents <&> C.lines

    let [r,c] = map readInt (C.words rc)
        rng = ((1,1),(r,c))
        grid = init rest
            & concatMap C.unpack
            & listArray rng
        [sy,sx] = last rest
            & C.words
            & map readInt

    print $ runST $ do
        seen <- newArray rng False
        writeArray seen (sy,sx) True
        search grid seen (singleton (1,(sy,sx)))

search :: UArray (Int,Int) Char -> STUArray s (Int,Int) Bool -> Seq (Int,(Int,Int)) -> ST s Int
search grid seen Empty      = pure undefined
search grid seen ((d,(y,x)) :<| xs)
    | any (inRange (bounds grid) >>> not) ns = pure d
    | otherwise = do
        (ds,cs) <- ns
            & filter ((grid!) >>> (/='#'))
            & filterM (\v -> readArray seen v >>= \s -> unless s (writeArray seen v True) >> pure (not s))
            <&> partition ((grid!) >>> (=='D'))
        search grid seen (fromList (map (d,) ds) <> xs <> fromList (map (d+1,) cs))
  where
    ns = [(y-1,x),(y+1,x),(y,x-1),(y,x+1)]

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
