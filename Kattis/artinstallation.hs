import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,h,[rg,gb]] <- getContents <&> (lines >>> map (words >>> map read))
    let [nr,ng,nb] = zipWith ((-) >>> (>>> max 0)) n h
    print $ if nr <= rg && nb <= gb && ng <= (rg - nr + gb - nb)
        then sum [nr,ng,nb]
        else -1
