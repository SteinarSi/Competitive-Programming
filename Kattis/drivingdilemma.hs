import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [s,d,t] <- getContents <&> (words >>> map read)
    putStrLn $ if d / (s * 5280 / 3600) <= t
        then "MADE IT"
        else "FAILED TEST"
