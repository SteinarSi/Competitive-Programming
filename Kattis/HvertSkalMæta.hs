main :: IO ()
main = do
    inn <- getLine
    putStrLn $ case inn of
        "Reykjavik" -> r 
        "Kopavogur" -> r 
        "Hafnarfjordur" -> r 
        "Reykjanesbaer" -> r 
        "Akureyri" -> a 
        "Gardabaer" -> r 
        "Mosfellsbaer" -> r 
        "Arborg" -> r 
        "Akranes" -> r 
        "Fjardabyggd" -> a 
        "Mulathing" -> a 
        "Seltjarnarnes" -> r
    where r = "Reykjavik"
          a = "Akureyri"
