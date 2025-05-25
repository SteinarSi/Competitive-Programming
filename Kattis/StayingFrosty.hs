import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [w,p] <- getLine <&> (words >>> map read)

    putStrLn $ if p * 180 * 2 >= w
        then "YES"
        else "NO"
