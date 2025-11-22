import           Control.Arrow ((>>>))
import           Data.Char     (digitToInt)
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    (s,m,l) <- getContents <&> (lines >>> drop 1 >>> count (0,0,0))
    print ((s+5) `div` 6 + (m+7) `div` 8 + (l+11) `div` 12)

count :: (Int,Int,Int) -> [String] -> (Int,Int,Int)
count (s,m,l) [] = (s,m,l)
count (s,m,l) ((c:_:x):xs) = case c of
    'S' -> count (s+a,m,l) xs
    'M' -> count (s,m+a,l) xs
    'L' -> count (s,m,l+a) xs
  where
    a = read x
