import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.Ix       (inRange)

main :: IO ()
main = do
    [x1,x2,y1,y2]:_:xs <- getContents <&> (lines >>> map (words >>> map (read::String->Int)))

    xs
        & filter (\[x,y] -> inRange ((x1,y1),(x2,y2)) (x,y))
        & length
        & print
