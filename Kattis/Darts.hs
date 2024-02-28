import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM, replicateM_)
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on, (&))
import           Data.Functor          ((<&>))
import           Data.List             (minimumBy)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    t:_ <- readInts
    replicateM_ t $ do
        n:_ <- readInts
        replicateM n readInts >>= (
                map (\(a:b:_) -> dart (a,b))
                >>> sum
                >>> print
            )

dart :: (Int,Int) -> Int
dart (x,y) = [1..10]
    & map (\p -> (p, (20*(11-p))^2))
    & filter (snd >>> (x^2 + y^2<=))
    & ((0,maxBound):)
    & minimumBy (compare `on` snd)
    & fst

readInts :: IO [Int]
readInts = C.getLine <&> (C.words >>> map (C.readInt >>> fromJust >>>  fst))
