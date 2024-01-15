import           Data.IntSet (IntSet, fromList, singleton, size, unions)

main :: IO ()
main = getLine >>= print . size . unions . map (toSet . map read . split ('-'==)) . split (';'==)

toSet :: [Int] -> IntSet
toSet [x]    = singleton x
toSet [x, y] = fromList [x..y]
toSet xs     = error (show xs)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
    [] -> []
    ys -> let (w, zs) = break p ys
          in  w : split p zs
