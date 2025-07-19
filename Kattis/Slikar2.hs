import           Control.Arrow         (second, (&&&), (***), (>>>))
import           Data.Array.Unboxed    (Ix (..), UArray, listArray, (!))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    Just (n,rest) <- C.getContents <&> C.readInt
    let grid = listArray ((1,1),(n,n)) (map ('1'==) (filter isDigit (C.unpack rest)))
    putStr (solve n grid)

solve :: Int -> UArray (Int,Int) Bool -> String
solve n grid = choose ((1,1),(n,n))
    & show
        ***
      (map (bool '0' '1') >>> chunksOf n)
    & uncurry (:)
    & unlines
  where
    choose :: ((Int,Int),(Int,Int)) -> (Int,[Bool])
    choose ((sx,sy),(tx,ty))
        | (sx,sy) == (tx,ty) = (0, [grid ! (sx,sy)])
        | otherwise          = map (zipWith (&) children
                >>> map (second (chunksOf (hx+1-sx)))
                >>> (\[(a,as),(b,bs),(c,cs),(d,ds)] -> (a+b+c+d, concat (zipWith (<>) as bs <> zipWith (<>) cs ds)))
                ) choices
            & minimum
      where
        size = 1+tx-sx
        hx = (tx+sx) `div` 2
        hy = (ty+sy) `div` 2
        children = [
            ((sx, sy),(hx, hy)),
            ((sx, hy + 1), (hx, ty)),
            ((hx + 1, sy), (tx, hy)),
            ((hx + 1, hy + 1), (tx, ty))
            ]
            & map (\rng -> (choose rng, white rng, black rng))
        choices = [
                [t1, t1, t2, t3],
                [t1, t1, t3, t2],
                [t1, t2, t1, t3],
                [t1, t2, t3, t1],
                [t1, t3, t1, t2],
                [t1, t3, t2, t1],
                [t2, t1, t1, t3],
                [t2, t1, t3, t1],
                [t2, t3, t1, t1],
                [t3, t1, t1, t2],
                [t3, t1, t2, t1],
                [t3, t2, t1, t1]
            ]

    white :: ((Int,Int),(Int,Int)) -> (Int,[Bool])
    white rng = (sum [1 | r <- range rng, not (grid!r)], replicate (rangeSize rng) True)

    black :: ((Int,Int),(Int,Int)) -> (Int,[Bool])
    black rng = white rng
        & (rangeSize rng -)
            ***
          map not

t1 :: (a,a,a) -> a
t1 (a,_,_) = a

t2 :: (a,a,a) -> a
t2 (_,a,_) = a

t3 :: (a,a,a) -> a
t3 (_,_,a) = a

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
