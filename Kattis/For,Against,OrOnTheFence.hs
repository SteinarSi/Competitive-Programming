import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [xp,yp,xv,yv,u] <- getContents <&> (words >>> map read)
    putStrLn $ case compare ((xp-xv)^2 + (yp-yv)^2) u of
        LT -> "for"
        EQ -> "on the fence"
        GT -> "against"
