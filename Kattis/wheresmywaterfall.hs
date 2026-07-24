import           Control.Arrow         (second, (>>>))
import           Control.Monad         (when)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.ST         (STUArray, getAssocs, getElems, inRange,
                                        newListArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    nmk:ws:xs <- C.getContents <&> C.lines
    let [n,m,_] = map readInt (C.words nmk)
        sources = map (readInt >>> succ >>> (1,)) (C.words ws)
        rng = ((1,1),(n,m))
        ans = runST $ do
            grid <- newListArray rng (concatMap C.unpack xs)
            mapM_ (flip (writeArray grid) '~') sources
            mapM_ (flow rng grid) sources
            getElems grid
    ans
        & chunksOf m
        & unlines
        & putStr

flow :: ((Int,Int),(Int,Int)) -> STUArray s (Int,Int) Char -> (Int,Int) -> ST s ()
flow rng grid (y,x)
    | not (inRange rng (y+1,x)) = pure ()
    | otherwise = do
        f <- readArray grid (y+1,x)
        case f of
            '~' -> pure ()
            'O' -> writeArray grid (y+1,x) '~' >> flow rng grid (y+1,x)
            _   -> do
                when (inRange rng (y,x-1)) $ do
                    l <- readArray grid (y,x-1)
                    when (l == 'O') $ writeArray grid (y,x-1) '~' >> flow rng grid (y,x-1)
                when (inRange rng (y,x+1)) $ do
                    l <- readArray grid (y,x+1)
                    when (l == 'O') $ writeArray grid (y,x+1) '~' >> flow rng grid (y,x+1)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
