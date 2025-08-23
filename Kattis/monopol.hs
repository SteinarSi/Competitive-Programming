import           Data.List  (group, sort)
import           Data.Maybe (fromJust)
import           Text.Printf (printf)

main :: IO ()
main = do
    _:xs <- fmap (map read . words) getContents
    printf "%.5f\n" $ sum (map (fromJust . (`lookup` combs)) xs) / (size * size)

size :: Double
size = 6

combs :: [(Double, Double)]
combs = map (\g -> (head g, fromIntegral $ length g)) . group $ sort [ p+q | p <- [1..size], q <- [1..size] ]
