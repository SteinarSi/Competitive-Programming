import           Data.Bool (bool)
import           Data.List (nub)

main :: IO ()
main = do
    [from, to] <- fmap (map read . words) getLine
    print (interval from to)

start :: Int
start = 123456

end :: Int
end = 987654

valid :: Int -> Bool
valid x = notElem 0 d && nub d == d && all ((==0) . mod x) d
    where d = digits x

digits :: Int -> [Int]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

psum :: [Int]
psum = scanl ((. bool 0 1 . valid) . (+)) 0 [start..end]

interval :: Int -> Int -> Int
interval from to = psum !! (1 + to - start) - psum !! (from - start)
