import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (elemIndex)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    [x,y] <- getContents <&> (words >>> map ((`elemIndex` ["N", "NE", "E", "SE", "S", "SW", "W", "NW"]) >>> fromJust))
    let r = (y - x) `mod` 8
        l = (x - y) `mod` 8
    putStrLn $ case compare l r of
        LT             -> "Turn " <> show (l * 45) <> " degrees port"
        GT             -> "Turn " <> show (r * 45) <> " degrees starboard"
        EQ | x==y      -> "Keep going straight"
           | otherwise -> "U-turn"
