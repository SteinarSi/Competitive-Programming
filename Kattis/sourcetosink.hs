import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [l,w,v] <- getLine <&> (words >>> map read)
    print (4*((l*w+v-1) `div` v)-1)
