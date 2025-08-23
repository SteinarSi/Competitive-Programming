import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> parser (0,0,0,0) (0,0,0,0)
        >>> solve
        >>> putStr
    )

solve :: ((Int,Int,Int,Int), (Int,Int,Int,Int)) -> String
solve ((n,a,b,c),(m,x,y,z)) = unlines [efficacy a x, efficacy b y, efficacy c z]
  where
    efficacy u v = let without = rate m v
                       with    = rate n u
                   in  if with >= without
                            then "Not Effective"
                            else show (100 * (rate m v - rate n u) / rate m v)
    rate k u = fromIntegral u / fromIntegral k

parser :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> [C.ByteString] -> ((Int,Int,Int,Int), (Int,Int,Int,Int))
parser w wo []                         = (w,wo)
parser w@(n,a,b,c) wo@(m,x,y,z) (s:ss) | C.head s == 'Y' = parser (n+1, a + yes 1, b + yes 2, c + yes 3) wo ss
                                       | otherwise       = parser w (m+1, x + yes 1, y + yes 2, z + yes 3) ss
  where
    yes i = bool 0 1 (C.index s i == 'Y')
