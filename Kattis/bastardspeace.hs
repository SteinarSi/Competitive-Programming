import           Control.Arrow         (second, (&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readInt)
        >>> (head >>> last) &&& (drop 1 >>> chunksOf 3 >>> zipWith (\i [_,ps@(p:_),ts] -> (p*1000000000+i, (zip ps ts, i))) [1..] >>> M.fromList)
        >>> uncurry solve
        >>> putStrLn
    )

solve :: Int -> M.IntMap ([(Int,Int)],Int) -> String
solve s floors = case M.minView floors of
    Nothing                                   -> "Sigur!"
    Just (([],_), floors')                    -> solve s floors'
    Just (([(p,t)],_), floors')
        | p >= s                              -> show s
        | otherwise                           -> solve (s+t) floors'
    Just (((p1,t1):xs@((p2,_):_),i), floors')
        | p1 >= s                             -> show s
        | otherwise                           -> solve (s+t1) (M.insert (p2*1000000000+i) (xs,i) floors')

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
