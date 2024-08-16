import           Control.Arrow         ((***))
import           Control.Category      ((>>>))
import           Data.Array            (Array, bounds, inRange, listArray,
                                        range, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.readInt
            >>> fromJust
            >>> fst
            >>> ((0,0),)
            >>> (dp!)
            >>> show
            >>> C.pack)
        >>> C.unlines
        >>> C.putStr
    )

dp :: Array ((Int,Int), Int) Int
dp = listArray (((-14,-14),0), ((14,14),14)) (map f (range (((-14,-14),0), ((14,14),14))))
    where
        f ((0,0), 0) = 1
        f (_, 0) = 0
        f ((p,q),i) | max (abs p) (abs q) > i = 0
                    | otherwise = [(1,0), (1,-1), (0,-1), (-1,0), (-1,1), (0,1)]
                        & map ((p+) *** (q+))
                        & filter (inRange (bounds dp & fst *** fst))
                        & map ((,i-1) >>> (dp!))
                        & sum
