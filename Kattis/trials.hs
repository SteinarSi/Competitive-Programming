import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b,c] <- getContents <&> (words >>> map read)
    let s = (a+b+c) / 2
    print (sqrt (s * (s-a) * (s-b) * (s-c)))
