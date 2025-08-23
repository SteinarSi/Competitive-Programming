main :: IO ()
main = getLine >>= mapM_ print . smil 0

smil :: Int -> String -> [Int]
smil _ ""               = []
smil i (':':')':xs)     = i : smil (i+2) xs
smil i (';':')':xs)     = i : smil (i+2) xs
smil i (':':'-':')':xs) = i : smil (i+3) xs
smil i (';':'-':')':xs) = i : smil (i+3) xs
smil i (_:xs)           = smil (i+1) xs
