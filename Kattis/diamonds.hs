import           Control.Arrow (second, (>>>))
import           Data.Bool     (bool)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import qualified Data.IntSet   as S
import           Data.Ix       (range)

main :: IO ()
main = do
    [r,c] <- getLine <&> (words >>> map read)
    putStr (format (r,c) (solve (r,c)))

solve :: (Int,Int) -> [(Int,Int)]
solve (r,c) = [(y,x) | y <- ys, x <- xs]
  where
    ys | r == 2 = [1,2]
       | r == 3 = [1,2]
       | r `mod` 5 == 2 = r-2 : concatMap (\i -> [i,i+1]) [3,8..r-1]
       | r `mod` 5 == 3 = r-1 : r-2 : concatMap (\i -> [i,i+1]) [3,8..r-1]
       | otherwise      = concatMap (\i -> [i,i+1]) [3,8..r-1]

    xs | c `mod` 4 == 2 = c : [3,7..c]
       | otherwise = [3,7..c]

format :: (Int,Int) -> [(Int,Int)] -> String
format (r,c) bans = range ((1,1),(r,c))
    & map (hash >>> (`S.member` set) >>> bool '.' 'X')
    & chunksOf c
    & unlines
  where
    hash (y,x) = 100*y + x
    set = S.fromList (map hash bans)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = splitAt k xs
    & second (chunksOf k)
    & uncurry (:)
