import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,k] <- getContents <&> (words >>> map read)

    let win = n `mod` (k+1) /= 0

    putStrLn $ if win
        then "Jebb"
        else "Neibb"
