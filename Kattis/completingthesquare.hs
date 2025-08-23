import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    [a,b,x,y,p,q] <- C.getContents <&> (C.words >>> map readInt)
    let t@((cx,cy), (dx,dy), (ex,ey)) = order (a,b) (x,y) (p,q)
    putStrLn . unwords $ map show [dx + ex - cx, dy + ey - cy]

order :: (Int,Int) -> (Int,Int) -> (Int,Int) -> ((Int,Int), (Int,Int), (Int,Int))
order ab xy pq | sqdist ab pq == sqdist ab xy = (ab, xy, pq)
               | sqdist pq ab == sqdist pq xy = (pq, ab, xy)
               | sqdist xy ab == sqdist xy pq = (xy, ab, pq)
               | otherwise = error "bruh"

sqdist :: (Int,Int) -> (Int,Int) -> Int
sqdist (a,b) (c,d) = abs (a-c) ^ 2 + abs (b-d) ^ 2

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
