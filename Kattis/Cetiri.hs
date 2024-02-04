import           Control.Arrow ((>>>))
import           Data.Function (on, (&))
import           Data.List     (find, maximumBy, sort)
import           Data.Maybe    (fromJust)

main :: IO ()
main = getLine >>= (words >>> map read >>> sort >>> lost >>> print)

lost :: [Int] -> Int
lost xs@(x:_) | diff == 0 = x
              | otherwise = find (`notElem` xs) [x, x+diff..] & fromJust
    where diffs = zipWith (-) (tail xs) xs
          diff = maximumBy (compare `on` (\d -> length (filter (d==) diffs))) diffs
