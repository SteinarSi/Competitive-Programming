import Control.Arrow ((>>>))
import Data.Functor  ((<&>))
import Data.List     (elemIndex)
import Data.Maybe    (fromJust)

main :: IO ()
main = do
    [from, to, other] <- getContents <&> (words >>> map ((`elemIndex` dirs) >>> fromJust))


    let wantToPassStraightThrough = abs (from - to) == 2
        otherCarFromTheRight = (from - other) `mod` 4 == 1
        wantToTurnLeft = (to - from) `mod` 4 == 1
        otherCarFromTheOpposite = abs (from - other) == 2
    
        cond1 = wantToPassStraightThrough && otherCarFromTheRight
        cond2 = wantToTurnLeft && (otherCarFromTheOpposite || otherCarFromTheRight)

    putStrLn $ if cond1 || cond2
        then "Yes"
        else "No"

dirs :: [String]
dirs = ["North", "East", "South", "West"]
