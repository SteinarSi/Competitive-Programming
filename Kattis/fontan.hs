import           Control.Arrow    (second, (>>>))
import           Control.Monad    (when)
import           Control.Monad.ST (ST, runST)
import           Data.Array.ST    (Ix (inRange), STUArray, getAssocs, getElems,
                                   newListArray, readArray, writeArray)
import           Data.Function    ((&))
import           Data.Functor     ((<&>))

main :: IO ()
main = do
    n:m:xs <- getContents <&> words

    let rng = ((1,1),(read n, read m))
        ans = runST $ do
            grid <- newListArray rng (concat xs)
            getAssocs grid >>= (filter (snd >>> (=='V')) >>> mapM_ (fst >>> flow rng grid))
            getElems grid

    ans
        & chunksOf (read m)
        & unlines
        & putStr

flow :: ((Int,Int),(Int,Int)) -> STUArray s (Int,Int) Char -> (Int,Int) -> ST s ()
flow rng grid (y,x)
    | not (inRange rng (y+1,x)) = pure ()
    | otherwise = do
        f <- readArray grid (y+1,x)
        case f of
            'V' -> pure ()
            '.' -> writeArray grid (y+1,x) 'V' >> flow rng grid (y+1,x)
            '#' -> do
                when (inRange rng (y,x-1)) $ do
                    l <- readArray grid (y,x-1)
                    when (l == '.') $ writeArray grid (y,x-1) 'V' >> flow rng grid (y,x-1)
                when (inRange rng (y,x+1)) $ do
                    l <- readArray grid (y,x+1)
                    when (l == '.') $ writeArray grid (y,x+1) 'V' >> flow rng grid (y,x+1)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
