import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    b:ps <- getContents <&> (words >>> map read)
    putStrLn $ if b >= sum ps
        then "Budget is sufficient."
        else "Budget is insufficient."
