main :: IO ()
main = interact translate

translate :: String -> String
translate ""                                   = ""
translate ('b':'a':'u':'k':xs)                 = "dos" <> translate xs
translate ('f':'l':'a':'t':'b':'a':'k':'a':xs) = "petsa" <> translate xs
translate ('k':xs)                             = 'g' : translate xs
translate ('y':xs)                             = 'u' : translate xs
translate (x:xs)                               = x : translate xs
