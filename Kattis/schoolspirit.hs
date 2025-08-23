import           Data.List (delete)

main :: IO ()
main = do
    n:xs <- fmap (map read . words) getContents
    print (score xs)
    print (sum (map (\x -> score (delete x xs)) xs) / n)

score :: [Double] -> Double
score = (/5) . sum . zipWith (*) (map ((4/5)**) [0..])
