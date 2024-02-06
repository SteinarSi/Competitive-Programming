import           Control.Arrow      ((>>>))
import           Control.Monad      (replicateM, when)
import           Data.Array.Unboxed (IArray (bounds), UArray, array, (!), (//))
import           Data.Functor       ((<&>))
import           Data.List          (find)
import           Data.Maybe         (fromJust)

main :: IO ()
main = loop 1

loop :: Int -> IO ()
loop t = do
    [n, m] <- fmap (map read . words) getLine
    when (n/=0 && m /= 0) $ do
        house <- fmap (array ((1,1),(n,m)) . concat . zipWith (\y xs -> zipWith (\x c -> ((x, y), c)) [1..] xs) [1..]) (replicateM m getLine)
        let end = reflect house (findStart house)
            newHouse = house // [(end, '&')]
            rows = map (\y -> map (\x -> newHouse ! (x,y)) [1..n]) [1..m]
        putStrLn ("HOUSE " ++ show t)
        putStr (unlines rows)
        loop (t+1)

data Dir = North | South | East | West
type Pos = (Int, Int)

reflect :: UArray (Int,Int) Char -> (Pos, Dir) -> Pos
reflect house (pos@(x,y),dir) = case (house ! pos, dir) of
    ('x',  _    ) -> pos
    ('/',  North) -> reflect house ((x+1, y  ), East )
    ('/',  South) -> reflect house ((x-1, y  ), West )
    ('/',  East ) -> reflect house ((x  , y-1), North)
    ('/',  West ) -> reflect house ((x  , y+1), South)
    ('\\', North) -> reflect house ((x-1, y  ), West )
    ('\\', South) -> reflect house ((x+1, y  ), East )
    ('\\', East ) -> reflect house ((x  , y+1), South)
    ('\\', West ) -> reflect house ((x  , y-1), North)
    (_,    North) -> reflect house ((x  , y-1), North)
    (_,    South) -> reflect house ((x  , y+1), South)
    (_,    East ) -> reflect house ((x+1, y  ), East )
    (_,    West ) -> reflect house ((x-1, y  ), West )

findStart :: UArray (Int,Int) Char -> (Pos, Dir)
findStart house = fromJust $ find (('*' ==) . (house!) . fst) $ concat [
        [ ((x,1), South) | x <- [2..n-1] ],
        [ ((x,m), North) | x <- [2..n-1] ],
        [ ((1,y), East ) | y <- [2..m-1] ],
        [ ((n,y), West ) | y <- [2..m-1] ]
    ]
    where (n,m) = snd (bounds house)
