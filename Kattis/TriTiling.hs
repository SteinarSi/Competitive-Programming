import           Control.Arrow ((>>>))
import           Data.Array    (Array, listArray, (!))
import           Data.Ix       (range)
import           Data.Maybe    (fromJust)

main :: IO ()
main = getContents >>= (
            lines
        >>> init
        >>> map (read >>> (\n -> tiles ! (n,n,n)) >>> show)
        >>> unlines
        >>> putStr
    )

tiles :: Array (Int,Int,Int) Int
tiles = listArray r $ map f (range r)
    where
        r = ((-1,-1,-1),(30,30,30))

        f (0,0,0) = 1
        f (x,y,z) | x < 0 || y < 0 || z < 0 = 0
                  | x > max y z = tiles ! (x-2,y,z)
                  | y > max x z = tiles ! (x,y-2,z)
                  | z > max x y = tiles ! (x,y,z-2)
                  | x == y && x == z = tiles ! (x-2,y-1,z-1) + tiles ! (x-1,y-1,z-2) + tiles ! (x-2,y-2,z-2)
                  | x < max y z = tiles ! (x,y-1,z-1) + tiles ! (x,y-2,z-2)
                  | z < max x y = tiles ! (x-1,y-1,z) + tiles ! (x-2,y-2,z)
                  | y < max x y = tiles ! (x-2,y,z-2)
                  | otherwise = error ("Bruh: " <> show (x,y,z))
