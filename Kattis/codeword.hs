import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed (Array, UArray, elems, listArray, range,
                                     (!))
import           Data.Bool          (bool)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))

main :: IO ()
main = do
    [r,c,l] <- getContents <&> (words >>> map read)

    let
        rng = ((1,1),(r,c))

        noptions :: Array (Int,Int) [(Int,Int)]
        noptions = listArray rng (map f (range rng))
          where
            f (y,x) = range ((minY,minX),(maxY,maxX))
              where
                minX = bool x (x-1) (x>1)
                maxX = bool x (x+1) (x<c)
                minY = bool y (y-1) (y>1)
                maxY = bool y (y+1) (y<r)

        next :: UArray (Int,Int) Int -> UArray (Int,Int) Int
        next prev = range rng
            & map ((noptions!) >>> map (prev!) >>> sum >>> (total-) >>> modulo)
            & listArray rng
          where
            total = sum (elems prev)

    repeat 1
        & listArray rng
        & iterate next
        & (!! (l-1))
        & elems
        & sum
        & modulo
        & print

modulo :: Int -> Int
modulo = (`mod` 1000000007)
