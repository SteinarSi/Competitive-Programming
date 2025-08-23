import           Control.Arrow         ((>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, bounds, listArray,
                                        newArray, readArray, writeArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Ix               (Ix (inRange), range)
import           Data.List             (find)
import           Data.Maybe            (fromJust)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parse
        >>> map (solve >>> bool "No" "Yes")
        >>> unlines
        >>> putStr
    )

data Die = Die {
    up :: !Int,
    fw :: !Int,
    lf :: !Int
} deriving (Eq, Ord, Show, Ix)

solve :: (Int, (Int,Int), (Int,Int), UArray (Int,Int) Bool) -> Bool
solve (n, s,t,grid) = runST $ do
    seen <- newArray (((1,1),Die 1 1 1), ((n,n),Die 6 6 6)) False
    bfs seen (Seq.singleton (s, Die { up = 1, lf = 5, fw = 4 }))

  where
    bfs :: STUArray s ((Int,Int),Die) Bool -> Seq ((Int,Int),Die) -> ST s Bool
    bfs seen Empty      = pure False
    bfs seen (u :<| xs) = do
        ys <- moves u
            & filter (fst >>> inRange (bounds grid))
            & filter (fst >>> (grid!))
            & filterM (\v -> readArray seen v >>= \s ->
                if s
                    then pure False
                    else writeArray seen v True >> pure True
                )
        if any (\(v,d) -> v == t && op (up d) == 5) ys
            then pure True
            else bfs seen (xs <> Seq.fromList ys)

moves :: ((Int,Int),Die) -> [((Int,Int),Die)]
moves ((y,x), Die {up,fw,lf}) = [
        ((y-1,x),Die {up = op fw, fw = up, lf = lf}),
        ((y+1,x),Die {up = fw, fw = op up, lf = lf}),
        ((y,x+1),Die {up = lf, fw = fw, lf = op up}),
        ((y,x-1),Die {up = op lf, fw = fw, lf = up})
    ]

op :: Int -> Int
op = (7-)

parse :: [C.ByteString] -> [(Int, (Int,Int), (Int,Int), UArray (Int,Int) Bool)]
parse []     = []
parse (x:xs) = (n, s, t, grid) : parse b
  where
    n = readInt x
    (a,b) = splitAt n xs
    content = C.unpack (C.concat a)
    rng = ((1,1),(n,n))
    ixs = zip (range rng) content
    Just (s,_) = find (snd >>> (=='S')) ixs
    Just (t,_) = find (snd >>> (=='H')) ixs
    grid = listArray rng (map (/='*') content)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
