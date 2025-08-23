main :: IO ()
main = interact test

test :: String -> String
test ('C':'O':'V':xs) = "Veikur!"
test (x:y:z:xs) = test (y : z : xs)
test _ = "Ekki veikur!"
