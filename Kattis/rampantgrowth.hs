import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [rows, cols] <- getLine <&> (words >>> map read)
    print $ iterate (((rows-1) *) >>> (`mod` 998244353)) rows !! (cols-1)
