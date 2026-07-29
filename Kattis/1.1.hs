import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))
import           Data.List     (sort)

main :: IO ()
main = do
    xs <- getLine <&> (words >>> map (read::String->Double))
    putStrLn $ if length xs < 3
        then "At least 3 scores needed!"
        else "Sum of scores (3 lowest removed): " <> show (sum (drop 3 (sort xs)))
