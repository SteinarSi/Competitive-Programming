import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [k,m,n] <- getContents <&> (words >>> map read)

    putStrLn $ if (k `mod` (m+n)) >= m
        then "Alex"
        else "Barb"
