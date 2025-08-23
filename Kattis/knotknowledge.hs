import           Data.IntSet (difference, findMin, fromList)

main :: IO ()
main = getLine >> difference <$> r <*> r >>= print . findMin
    where r = fmap (fromList . map read . words) getLine
