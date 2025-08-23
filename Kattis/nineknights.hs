import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed (UArray, array, bounds, indices, listArray,
                                     (!))
import           Data.Bifunctor     (bimap)
import           Data.Bool          (bool)
import           Data.Function      ((&))
import           Data.Ix            (inRange)

main :: IO ()
main = getContents >>= (
            lines
        >>> concat
        >>> map (=='k')
        >>> listArray ((1,1),(5,5))
        >>> solve
        >>> bool "invalid" "valid"
        >>> putStrLn
    )

solve :: UArray (Int,Int) Bool -> Bool
solve board = length knights == 9 && all (moves >>> map (board!) >>> or >>> not) knights
    where
        knights :: [(Int,Int)]
        knights = indices board & filter (board!)

        moves :: (Int,Int) -> [(Int,Int)]
        moves (x,y) = [(-2,-1), (-1,-2),(1,-2),(2,-1),(2,1),(1,2),(-1,2),(-2,1)]
                & map (bimap (x+) (y+))
                & filter (inRange (bounds board))
