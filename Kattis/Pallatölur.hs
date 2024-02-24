import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.Bool (bool)

main :: IO ()
main = do
    [a,b] <- getContents <&> (lines >>> map read)
    putStrLn $ bool ":(" "1\n2" (a <= 2 && 2 <= b)