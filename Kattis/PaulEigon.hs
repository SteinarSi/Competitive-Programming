import           Control.Arrow ((>>>))

main :: IO ()
main = do
    [n,p,q] <- fmap (words >>> map read) getLine
    putStrLn $ if even ((p + q) `div` n)
        then "paul"
        else "opponent"

