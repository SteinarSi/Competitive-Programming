import Control.Arrow ((>>>))
import Data.Functor ((<&>))

main :: IO ()
main = do
    [a,b] <- getContents <&> (words >>> map read)
    print (gcd a b)