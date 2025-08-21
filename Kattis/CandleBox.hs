import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (find)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    [d,r,t] <- getContents <&> (words >>> map read)

    scanl (\(x,y) i -> (x+i,y+bool 0 (i-d) (i-d >= 3))) (0,0) [4..]
        & find (uncurry (+) >>> (==r+t))
        & fromJust
        & fst
        & (r-)
        & print
