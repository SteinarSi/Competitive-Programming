import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.List             (foldl', maximumBy, sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [n]:[x]:rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let (ess,_:rss) = splitAt x rest
        (es,ep1,ep2,ep3) = scoring ess
        (rs,rp1,rp2,rp3) = scoring rss

        cmp :: Int -> Int -> String
        cmp e r
            | es > rs || es == rs && e < r = "YES"
            | otherwise                    = "NO"

    putStrLn (unwords [cmp ep1 rp1, cmp ep2 rp2, cmp ep3 rp3])

scoring :: [[Int]] -> (Int,Int,Int,Int)
scoring = map (\[p,s,t] -> (p,[(t,s)]))
    >>> M.fromListWith (<>)
    >>> M.elems
    >>> map problem
    >>> foldl' (\(a,b,c,d) (x,y,z,w) -> (a+x, b+y, c+z, max d w)) (0,0,0,0)
  where
    problem :: [(Int,Int)] -> (Int,Int,Int,Int)
    problem xs = (points, p1, p2, p3)
      where
        (time,points) = maximumBy (compare `on` (\(t,s) -> (s,-t))) xs
        p1 | points <= 0 = 0
           | otherwise   = time + 20 * length (filter (fst >>> (<time)) xs)
        p2 = length xs
        p3 | points <= 0 = 0
           | otherwise   = time

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
