import qualified Data.ByteString.Char8 as C
import           Control.Arrow            ((>>>))
import           Data.Bool                (bool)
import           Data.Function            (on)
import           Data.Ratio               ((%))
import           Data.Char                (isAlpha)

main :: IO ()
main = C.getContents >>= (
            C.filter isAlpha
        >>> C.reverse
        >>> C.foldl' guillaume ((minBound,maxBound,maxBound),(0,0,0))
        >>> fst
        >>> (\(g,a,_) -> show g ++ "-" ++ show a)
        >>> putStrLn
    )

guillaume :: ((Int,Int,Int),(Int,Int,Int)) -> Char -> ((Int,Int,Int),(Int,Int,Int))
guillaume (best,(g,a,d)) c = (next,curr)
    where 
        next | on (<=) (\(g',a',d') -> (bool (g' % (a'+g')) (0 % 1) (a'+g'==0), -(g'+a'+d'))) best curr = curr
             | otherwise = best
        curr = case c of
            'G' -> (g+1,a,d)
            'A' -> (g,a+1,d)
            _   -> (g,a,d+1)
