import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (find)

main :: IO ()
main = do
    n <- getLine <&> read
    let Just x = find ((^2) >>> show >>> notElem '6') [n+1..]
    print x
