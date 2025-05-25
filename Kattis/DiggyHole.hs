import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,h,x,m,y] <- getContents <&> (words >>> map read)
    print (y / (((x / n) / h) * m))
