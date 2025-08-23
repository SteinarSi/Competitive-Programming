import           Data.List (findIndex)

main :: IO ()
main = do
    [kind,h] <- words <$> getLine
    let p = case kind of
            "residential" -> [1,5,10,15,20]
            "commercial"  -> [1,7,14,20]
            "industrial"  -> [1,4,8,12,16,20]
        Just a = findIndex (>=read h) p
    print a
