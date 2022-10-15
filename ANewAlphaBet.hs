import Data.Char (toLower)

main :: IO ()
main = getLine >>= putStrLn . convert

alphabet :: [(Char, String)]
alphabet = [('a', "@"), ('b', "8"), ('c', "("), ('d', "|)"), ('e', "3"),
           ('f', "#"), ('g', "6"), ('h', "[-]"), ('i', "|"), ('j', "_|"),
           ('k', "|<"), ('l', "1"), ('m', "[]\\/[]"), ('n', "[]\\[]"),
           ('o', "0"), ('p', "|D"), ('q', "(,)"), ('r', "|Z"), ('s', "$"),
           ('t', "']['"), ('u', "|_|"), ('v', "\\/"), ('w', "\\/\\/"),
           ('x', "}{"), ('y', "`/"), ('z', "2")]

convert :: [Char] -> String
convert [] = ""
convert (c:xs) = case lookup (toLower c) alphabet of
    Just s -> s ++ convert xs
    Nothing -> c : convert xs