import Data.Functor ((<&>))
import Control.Arrow ((>>>))

main :: IO ()
main = do
    [bridges,knights,g] <- getContents <&> (words >>> map read)
    let groups = knights `div` g
        search = bridges - 1
    print ((search+groups-1) `div` groups)
