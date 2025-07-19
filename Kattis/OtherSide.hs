import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [w,s,c,k] <- getLine <&> (words >>> map read)

    putStrLn $ if s <= k && w+c <= 2*k || s < k || w+c < k || w+c <= k && s <= 2*k
        then "YES"
        else "NO"
