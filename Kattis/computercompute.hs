import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [x1,y1,x2,y2] <- getContents <&> (words >>> map read)
    print (sqrt ((x1-x2)^2 + (y1-y2)^2))
