import           Control.Arrow (first, second, (>>>))
import           Data.Function ((&))
import           Data.List     (transpose)
import           Data.Maybe    (fromMaybe)

main :: IO ()
main = do
    n':xs <- lines <$> getContents

    let n = read n'

    Just (concatMap (map (=='.')) (init xs), (replicate (n*n) Nothing, last xs))
        & iterate (>>= (\(grl,(board,msg)) -> (rotate n grl,) <$> replace (zip grl board) msg))
        & (!!4)
        >>= (snd >>> fst >>> sequence)
        & fromMaybe "invalid grille"
        & putStrLn

replace :: [(Bool,Maybe Char)] -> [Char] -> Maybe ([Maybe Char], [Char])
replace [] xs = Just ([],xs)
replace ((True,_):_) [] = Nothing
replace ((True,Just c) : cs) (x:xs)
    | c /= x    = Nothing
    | otherwise = first (Just x:) <$> (replace cs xs)
replace ((True,Nothing) : cs) (x:xs) = first (Just x:) <$> replace cs xs
replace (c:cs) xs = first (snd c:) <$> replace cs xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)

rotate :: Int -> [a] -> [a]
rotate n = chunksOf n >>> transpose >>> concatMap reverse
