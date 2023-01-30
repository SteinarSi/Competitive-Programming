import Data.Map (Map, (!), adjust, insert, singleton, empty)
import Data.Bool (bool)

type Directory = Map [String] [Either Integer String]

main :: IO ()
main = do
    dic <- fmap (parse empty [] . map words . lines) (readFile "inputs/day7-input.txt")
    let boundedSize = directory (const 0) (\d p -> (+bool 0 (size d p) (size d p <= 100000)) . sum) dic ["/"]
        deleteSize  = minimum $ filter (>=max 0 (size dic ["/"] - 40000000)) (directory (const []) (\d p -> (size d p:) . concat) dic ["/"])
    print (boundedSize, deleteSize)
    
parse :: Directory -> [String] -> [[String]] -> Directory
parse dic _ [] = dic
parse dic path (["$", "cd", ".."]:xs) = parse dic (tail path) xs
parse dic path (["$", "cd", name]:xs) = parse (insert (name:path) [] dic) (name:path) xs
parse dic path (["$", "ls"]:xs)       = parse dic path xs
parse dic path (["dir", name]:xs)     = parse (adjust (Right name:) path dic) path xs
parse dic path ([size, _]:xs)         = parse (adjust (Left (read size):) path dic) path xs

directory :: (Integer -> a) -> (Directory -> [String] -> [a] -> a) -> Directory -> [String] -> a
directory f g dic path = g dic path (map (either f (directory f g dic . (:path))) (dic ! path))

size :: Directory -> [String] -> Integer
size = directory id (\_ _ -> sum)