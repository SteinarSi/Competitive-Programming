import           Control.Arrow      ((>>>))
import           Control.Monad      (guard)
import           Data.Array.Unboxed (Ix (inRange), UArray, array, bounds, (!))

main :: IO ()
main = getContents >>= (
            lines
        >>> zipWith (\y -> zipWith (\x c -> ((x,y), c)) [1..]) [1..]
        >>> concat
        >>> array ((1,1), (7, 7))
        >>> peg
        >>> print
    )

peg :: UArray (Int,Int) Char -> Int
peg game = length $ do
    x <- [1..7]
    y <- [1..7]
    guard (game ! (x,y) == 'o')
    (dx,dy) <- [(-1,0), (1,0), (0,-1), (0,1)]
    guard (inRange (bounds game) (x+dx,y+dy))
    guard (game ! (x+dx,y+dy) == 'o')
    guard (inRange (bounds game) (x+dx*2,y+dy*2))
    guard (game ! (x+dx*2,y+dy*2) == '.')
    pure ()
