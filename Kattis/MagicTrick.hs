import Data.List (nub)

main :: IO ()
main = do
    inn <- getLine
    print $ if inn == nub inn
        then 1
        else 0
