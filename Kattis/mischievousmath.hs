import           Control.Arrow ((>>>))
import           Control.Monad (guard)
import           Data.Function ((&))
import           Data.List     (nub, permutations)
import           Data.Ratio    (Ratio, denominator, (%))

main :: IO ()
main = getLine >>= (read >>> solve >>> putStrLn)

solve :: Int -> String
solve n = head $ do
    a <- [1..100]
    guard (a /= n)
    b <- [1..100]
    guard (b /= n && b /= a)
    c <- [1..100]
    guard (c /= n && c /= a && c /= b)
    guard (not (valid [a,b,c]))
    pure (unwords (map show [a,b,c]))
  where
    valid = map (% 1) >>> combinations >>> elem (n%1)

combinations :: [Ratio Int] -> [Ratio Int]
combinations = permutations >>> concatMap combs
  where
    combs :: [Ratio Int] -> [Ratio Int]
    combs []     = [0]
    combs [x]    = [x]
    combs (x:xs) = nub ([id,(x+),(x*),(/x),\d -> if d /= 0 then x/d else -1,subtract x, (x-)] <*> combs xs)
