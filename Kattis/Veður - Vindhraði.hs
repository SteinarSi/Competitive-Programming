main :: IO ()
main = interact (beaufort . read) >> putChar '\n'

beaufort :: Float -> String
beaufort x | x <= 0.2 = "Logn"
           | x <= 1.5 = "Andvari"
           | x <= 3.3 = "Kul"
           | x <= 5.4 = "Gola"
           | x <= 7.9 = "Stinningsgola"
           | x <= 10.7 = "Kaldi"
           | x <= 13.8 = "Stinningskaldi"
           | x <= 17.1 = "Allhvass vindur"
           | x <= 20.7 = "Hvassvidri"
           | x <= 24.4 = "Stormur"
           | x <= 28.4 = "Rok"
           | x <= 32.6 = "Ofsavedur"
           | otherwise = "Farvidri"
