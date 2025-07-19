import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [xp,yp,xv,yv] <- getContents <&> (words >>> map read)
    print ((xp-xv)^2 + (yp-yv)^2)
