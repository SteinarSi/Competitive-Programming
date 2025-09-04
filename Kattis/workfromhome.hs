import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [w,m,c] <- getContents <&> (words >>> map read)
    print ((w*m*c+5999999) `div` 6000000)
