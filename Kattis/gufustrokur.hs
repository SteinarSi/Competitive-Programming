import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [x,y] <- getContents <&> (words >>> map read)
    print $ min ((x-y) `mod` 360) ((y-x) `mod` 360)
