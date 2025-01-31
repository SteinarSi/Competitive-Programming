import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [x1,y1,x2,y2] <- getContents <&> (words >>> map read)

    let corners = [(x1,y1),(x2,y2)]
            & filter (`elem` [(0,0),(0,2024),(2024,0),(2024,2024)])
            & length

    print $ case corners of
        1 -> 1
        2 -> 0
        _ -> 2
