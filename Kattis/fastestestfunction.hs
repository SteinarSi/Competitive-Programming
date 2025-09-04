import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [x,y] <- getLine <&> (words >>> map read)
    let r = 100 - x
        q = y * (r / (100-y))
    print (x / q)
