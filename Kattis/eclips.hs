import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    xs <- getContents <&> (words >>> filter ('e' `elem`) >>> unwords)
    putStrLn $ if null xs
        then "oh noes"
        else xs
