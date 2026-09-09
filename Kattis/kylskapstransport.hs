import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Text.Printf   (printf)

main :: IO ()
main = do
    [pa,ka,pb,kb,n] <- getLine <&> (words >>> map read)

    let cost :: Int -> (Int,Int,Int)
        cost i = let ta = ((n-i+ka-1) `div` ka)
                     tb = ((i+kb-1) `div` kb)
                 in  (pa*ta + pb*tb, ta, tb)
        (c,ta,tb) = minimum (map cost [0..n])

    printf "%d %d %d\n" ta tb c
