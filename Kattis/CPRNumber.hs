import Data.Char (digitToInt)
import Data.Bool (bool)
main = fmap (map digitToInt . filter (/='-')) getLine >>= print . bool 0 1 . (0==) . (`mod` 11) . sum . zipWith (*) [4,3,2,7,6,5,4,3,2,1]