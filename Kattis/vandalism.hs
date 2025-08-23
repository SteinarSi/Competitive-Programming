main :: IO ()
main = interact (\xs -> filter (`notElem` xs) "UAPC")
