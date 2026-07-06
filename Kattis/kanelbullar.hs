import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    (needs,has) <- getContents <&> (words >>> map read >>> splitAt 5)
    print (minimum (zipWith div has needs))
