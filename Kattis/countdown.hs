import           Control.Arrow (first, (>>>))
import           Data.Bool     (bool)
import           Data.Function (on)
import           Data.List     (minimumBy)
import           Text.Printf   (printf)

main :: IO ()
main = getContents >>= (
            lines
        >>> drop 1
        >>> map (words >>> map read >>> \xs -> solve (last xs) (init xs))
        >>> unlines
        >>> putStr
    )

solve :: Int -> [Int] -> String
solve t ns = unlines (printf "Target: %d" t : reverse rs <> [printf "Best approx: %d" b])
  where
    (b,rs) = minimumBy (compare `on` (fst >>> subtract t >>> abs)) (search ns)

    search :: [Int] -> [(Int,[String])]
    search [] = []
    search [x] = [(x,[])]
    search xs = map (,[]) xs <> (do
        (ys,zs) <- partitions xs
        (y,ry) <- search ys
        (z,rz) <- search zs
        [
            (y+z, printf "%d + %d = %d" y z (y+z) : ry <> rz),
            (y*z, printf "%d * %d = %d" y z (y*z) : ry <> rz)
            ]
            <> bool [] [(z-y, printf "%d - %d = %d" z y (z-y) : ry <> rz)] (z >= y)
            <> bool [] [(y-z, printf "%d - %d = %d" y z (y-z) : ry <> rz)] (y >= z)
            <> bool [] [(y `div` z, printf "%d / %d = %d" y z (y `div` z) : ry <> rz)] (z /= 0 && y `mod` z == 0)
            <> bool [] [(z `div` y, printf "%d / %d = %d" z y (z `div` y) : ry <> rz)] (y /= 0 && z `mod` y == 0)
        )

partitions :: [a] -> [([a],[a])]
partitions [] = error "bruh"
partitions (y:ys) = map (first (y:)) (drop 1 (ps ys))
  where
    ps :: [a] -> [([a],[a])]
    ps []     = []
    ps [x]    = [([x],[]),([],[x])]
    ps (x:xs) = concatMap (\(ys,zs) -> [(x:ys,zs),(ys,x:zs)]) (ps xs)
