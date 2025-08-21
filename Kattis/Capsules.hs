import           Control.Arrow         (first, second, (&&&), (***), (>>>))
import           Control.Monad         (filterM, forM_, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, array, getElems,
                                        listArray, newArray, newListArray,
                                        readArray, writeArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange, range)
import           Data.List             (partition)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    rc:rest <- C.getContents <&> (C.lines >>> map C.words)
    let [r,c] = map readInt rc
        rng = ((1,1),(r,c))
    splitAt r rest
        & concatMap (map (C.readInt >>> maybe 0 fst))
            ***
            (tail >>> zipWith (\i (_:xs) -> map (parseTuple >>> (,i)) xs) [1..])
        & uncurry (solve (r,c))
        & putStrLn

solve :: (Int,Int) -> [Int] -> [[((Int, Int), Int)]] -> String
solve (r,c) start regions = runST $ do
    has <- newArray ((1,1),(m,maximum (map length regions))) False
    forM_ filled (first (rs!) >>> flip (writeArray has) True)
    current <- newListArray rng start
    fill has current open
    getElems current <&> format
  where
    rng = ((1,1),(r,c))
    m = length regions
    (filled,open) = partition (snd >>> (/=0)) (zip (range rng) start)
        & second (map fst)

    rs :: UArray (Int,Int) Int
    rs = array rng (concat regions)

    sizes :: UArray Int Int
    sizes = listArray (1,m) (map length regions)

    fill :: forall s. STUArray s (Int,Int) Bool -> STUArray s (Int,Int) Int -> [(Int,Int)] -> ST s Bool
    fill has current []         = pure True
    fill has current ((x,y):xs) = anyM (\i -> do
            h <- readArray has (rg, i)
            n <- [(x-1,y),(x-1,y-1),(x,y-1),(x+1,y-1),(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1)]
                & filter (inRange rng)
                & anyM (readArray current >>> fmap (==i))
            if h || n
                then pure False
                else do
                    writeArray current (x,y) i
                    writeArray has (rg,i) True
                    f <- fill has current xs
                    unless f (writeArray current (x,y) 0 >> writeArray has (rg,i) False)
                    pure f) [1..sizes ! rg]
      where
        rg = rs ! (x,y)

    format :: [Int] -> String
    format = map show
        >>> chunksOf c
        >>> map unwords
        >>> unlines
        >>> init

parseTuple :: C.ByteString -> (Int,Int)
parseTuple x = (digitToInt (C.index x 1), digitToInt (C.index x 3))

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p []     = pure False
anyM p (x:xs) = p x >>= bool (anyM p xs) (pure True)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
        & second (chunksOf k)
        & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
