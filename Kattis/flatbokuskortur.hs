import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [x,y,z] <- getContents <&> (lines >>> map read)

    putStrLn $ if z * (pi * (y/2)^2) >= pi * (x/2)^2
        then "Jebb"
        else "Neibb"
