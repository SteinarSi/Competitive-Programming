import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    k:_:xs <- getContents <&> (words >>> map read)
    print (sum (map (k^) xs))
