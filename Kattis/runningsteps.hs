import           Control.Applicative   (liftA2)
import           Control.Arrow         ((&&&), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)
import           Data.STRef.Strict     (STRef, modifySTRef, newSTRef, readSTRef)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> head &&& last)
        >>> solve
        >>> map (uncurry (printf "%d %d"))
        >>> unlines
        >>> putStr
    )

solve :: [(Int,Int)] -> [(Int,Integer)]
solve xs = runST $ do
    ref <- newSTRef M.empty
    mapM (\(k,s) -> (k,) <$> stref ref (s,0,0,0)) xs

stref :: STRef s (M.IntMap Integer) -> (Int,Int,Int,Int) -> ST s Integer
stref _ (0,0,0,d) | d >= 0 = pure 1
stref ref (s,ds,dd,d)
    | s <= 0 = pure 0
    | abs ds > s = pure 0
    | abs dd > 2*s = pure 0
    | -(2 * d) > s = pure 0
    | otherwise = do
        let r = bool 1 (-1) (odd (ds+dd))
            h = hash (s,ds,dd,d)
        m <- readSTRef ref <&> M.lookup h
        case m of
            Just k -> pure k
            Nothing -> do
                k <- liftA2 (+) (stref ref (s-1,ds+r,dd,d-1)) (stref ref (s-2,ds,dd+r,d+1))
                modifySTRef ref (M.insert h k)
                pure k

hash :: (Int,Int,Int,Int) -> Int
hash (s,ds,dd,d) = s + 101 * (ds+50) + 10001 * (dd+25) + 1000001 * (d+50)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
