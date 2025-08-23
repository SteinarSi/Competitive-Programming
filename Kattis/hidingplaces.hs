import           Control.Monad    (filterM, replicateM_)
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (MArray (newArray), STUArray, getAssocs)
import           Data.Array.ST    (getAssocs, readArray, writeArray)
import           Data.Char        (chr, ord)
import           Data.Function    (on)
import           Data.Int         (Int8)
import           Data.List        (maximumBy, sortBy)
import           Data.Sequence    (Seq (..))
import qualified Data.Sequence    as Seq

main :: IO ()
main = do
    n <- fmap read getLine
    replicateM_ n $ do
        start <- fmap parsePosition getLine
        let result = runST $ do
                dist <- newArray (('a', 1), ('h', 8)) 99
                writeArray dist start 0
                result <- bfs (Seq.singleton start) dist
                formatResult result
        putStrLn result

parsePosition :: String -> (Char, Int8)
parsePosition [c,r] = (c, read [r])

formatResult :: STUArray s (Char, Int8) Int8 -> ST s String
formatResult dist = do
    dists <- getAssocs dist
    let best  = snd $ maximumBy (compare `on` snd) dists
        spots = map (\((c,r),_) -> c : show r) $ sortBy (\((r1,c1),_) ((r2,c2),_) -> compare (-c1,r1) (-c2,r2)) $ filter ((best ==) . snd) dists
    pure $ unwords (show best : spots)

bfs :: Seq (Char, Int8) -> STUArray s (Char, Int8) Int8 -> ST s (STUArray s (Char, Int8) Int8)
bfs Empty dist        = pure dist
bfs (u :<| rest) dist = do
    distu <- readArray dist u
    next <- filterM (\v -> do
            distv <- readArray dist v
            if distu + 1 < distv
                then writeArray dist v (distu + 1) >> pure True
                else pure False
        ) (moves u)
    bfs (rest <> Seq.fromList next) dist

moves :: (Char, Int8) -> [(Char, Int8)]
moves pos = filter (\(c,r) -> and [c >= 'a', c <= 'h', r >= 1, r <= 8]) $ map (pos +++) [(-2, -1), (-1, -2), (1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1)]

(+++) :: (Char, Int8) -> (Int, Int8) -> (Char, Int8)
(+++) (c1, r1) (dx, dy) = (chr (ord c1 + dx), r1 + dy)
