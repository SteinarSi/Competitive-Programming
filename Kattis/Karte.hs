import           Control.Monad (foldM)

main :: IO ()
main = getLine >>= putStrLn . maybe "GRESKA" counts . foldM delete allCards . chunksOf 3

delete :: Eq a => [a] -> a -> Maybe [a]
delete [] _ = Nothing
delete (x:xs) a | x == a = Just xs
                | otherwise = fmap (x:) (delete xs a)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)

allCards :: [String]
allCards = [suit : num | suit <- suits, num <- map (('0':) . show) [1..9] ++ map show [10..13]]

suits :: String
suits = "PKHT"

counts :: [String] -> String
counts cards = unwords $ map (show . (\s -> length (filter ((s==) . head) cards))) suits
