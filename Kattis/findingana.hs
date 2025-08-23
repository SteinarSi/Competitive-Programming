main :: IO ()
main = interact a

a :: String -> String
a ('a':xs) = 'a' : xs
a (_:xs) = a xs